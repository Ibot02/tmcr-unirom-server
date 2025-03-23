{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Entrances where

import System.IO (withFile, IOMode(AppendMode), Handle(), hPutStrLn)

import Control.Monad (forever, forM_)

import Data.Word

import Control.Concurrent.STM
import Control.Concurrent.Async (withAsync, cancel, wait)
import Control.Exception (finally)

import Control.Applicative

import Control.Lens

import Data.Binary.Get
import Data.Binary.Put

import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString as BS

import Protocol
import Handler

data Position = Position {
    _positionArea :: Word8
  , _positionRoom :: Word8
  , _positionLayer :: Word8
  , _positionX :: Word16
  , _positionY :: Word16
  } deriving Show

data Warp = Warp {
    _warpID :: Word32
  , _warpOldPosition :: Position
  , _warpNewPosition :: Position
  , _warpTileType :: Word16
  , _warpAnim :: Word8
  , _warpSpawnState :: Word8
  } deriving Show

$(makeLenses ''Position)
$(makeLenses ''Warp)

decodeWarp :: BS.ByteString -> Maybe Warp
decodeWarp = either (const Nothing) (\(_,_,w) -> Just w) . runGetOrFail getWarp . BL.fromStrict

getWarp :: Get Warp
getWarp = do
  _warpID <- getWord32le
  _warpOldPosition <- getPosition
  _warpNewPosition <-  getPosition
  _warpTileType <- getWord16le
  _warpAnim <- getWord8
  _warpSpawnState <- getWord8
  return $ Warp {..}

getPosition :: Get Position
getPosition = do
  _positionArea <- getWord8
  _positionRoom <- getWord8
  _positionLayer <- getWord8
  _ <- getWord8 -- padding
  _positionX <- getWord16le
  _positionY <- getWord16le
  return $ Position {..}

putPosition :: Position -> Put
putPosition (Position {..}) = do
  putWord8 _positionArea
  putWord8 _positionRoom
  putWord8 _positionLayer
  putWord8 0xFF --padding
  putWord16le _positionX
  putWord16le _positionY

putWarp :: Warp -> Put
putWarp (Warp {..}) = do
  putPosition _warpNewPosition
  putWord16le _warpTileType
  putWord8 _warpAnim
  putWord8 _warpSpawnState

encodeWarp :: Warp -> BS.ByteString
encodeWarp = BL.toStrict . runPut . putWarp

handleEntranceSwap :: Maybe FilePath -> Handler (TBQueue Packet, TQueue Packet) ()
handleEntranceSwap outputFile = withResourcesBrackets (withG outputFile) withR $ h

handleEntranceReflect :: Maybe FilePath -> Handler (TBQueue Packet, TQueue Packet) ()
handleEntranceReflect outputFile = withResourcesBrackets (writeToOutput outputFile) (const ($ ())) $ liftSTM $ \(output, (), (inQ, outQ)) -> ignoreAck Entrance inQ <|> do
  (addr, n, warp) <- handleWarp inQ outQ output
  sendWarp (addr, n) warp outQ

type G = (TMVar (Warp, TMVar Warp), Warp -> STM ())
type R = (TMVar Warp, TMVar (Word8, Word32))

withG :: Maybe FilePath -> (G -> IO a) -> IO a
withG file cb = do
  interconnect <- newEmptyTMVarIO
  writeToOutput file $ \output -> cb (interconnect, output)

writeToOutput :: Maybe FilePath -> ((Warp -> STM ()) -> IO a) -> IO a
writeToOutput Nothing cb = cb $ const $ return ()
writeToOutput (Just file) cb = do
  q <- newTQueueIO
  withFile file AppendMode $ \handle -> withAsync (writeAllTo q handle) $ \a -> flip finally (cancel a >> wait a) $ cb $ writeTQueue q --todo: check if the cancel >> wait is neccessary for finalization

writeAllTo :: (TQueue Warp) -> Handle -> IO ()
writeAllTo q handle =
  finally (forever $ do
    x <- atomically $ readTQueue q
    hPutStrLn handle $ show x) (do
    xs <- atomically $ flushTQueue q
    forM_ xs $ hPutStrLn handle . show)

withR :: G -> (R -> IO a) -> IO a
withR _ cb = do
  r <- (,) <$> newEmptyTMVarIO <*> newEmptyTMVarIO
  cb r

h :: Handler (G, R, (TBQueue Packet, TQueue Packet)) ()
h = liftSTM $ \i -> handleRead i <|> handleDelayedReply i

handleRead :: (G, R, (TBQueue Packet, TQueue Packet)) -> STM ()
handleRead ((interconnect, output), (w, p), (inQ, outQ)) = do
  (addr, n, warp) <- handleWarp inQ outQ output
  (do putTMVar interconnect (warp, w)
      putTMVar p (addr, n)
    ) <|> (do
      (warp', box) <- takeTMVar interconnect
      putTMVar box warp
      sendWarp (addr, n) warp' outQ
    )

handleWarp :: TBQueue Packet -> TQueue Packet -> (Warp -> STM ()) -> STM (Word8, Word32, Warp)
handleWarp inQ outQ output = do
  (addr, n, d) <- handlePacket Entrance inQ outQ
  case decodeWarp d of
    Just warp -> do
      output warp
      return (addr, n, warp)
    Nothing -> retry

handleDelayedReply :: (G, R, (TBQueue Packet, TQueue Packet)) -> STM ()
handleDelayedReply ((_interconnect, _output), (w, p), (_inQ, outQ)) = do
  (addr, n) <- takeTMVar p
  warp <- takeTMVar w
  sendWarp (addr, n) warp outQ

sendWarp :: (Word8, Word32) -> Warp -> TQueue Packet -> STM ()
sendWarp (addr, n) warp outQ = do
  let d = encodeWarp warp
  writeTQueue outQ $ DataPacket addr Entrance n d

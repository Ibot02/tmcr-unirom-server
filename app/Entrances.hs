{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RecordWildCards #-}
module Entrances where

import Data.Word

import Control.Concurrent.STM

import Control.Applicative

import Control.Lens

import Data.Binary.Get
import Data.Binary.Put

import Data.ByteString.Lazy (pack, unpack)

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

decodeWarp :: [Word8] -> Maybe Warp
decodeWarp = either (const Nothing) (\(_,_,w) -> Just w) . runGetOrFail getWarp . pack

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

encodeWarp :: Warp -> [Word8]
encodeWarp = unpack . runPut . putWarp

handleEntranceSwap :: Handler (TBQueue Packet, TQueue Packet) ()
handleEntranceSwap = withResources acqG relG acqR relR $ h

handleEntranceReflect :: Handler (TBQueue Packet, TQueue Packet) ()
handleEntranceReflect = liftSTM $ \(inQ, outQ) -> ignoreAck Entrance inQ <|> do
  (addr, n, warp) <- handleWarp inQ outQ
  sendWarp (addr, n) warp outQ

type G = (TMVar (Warp, TMVar Warp))
type R = (TMVar Warp, TMVar (Word8, Word32))

acqG :: IO G
acqG = newEmptyTMVarIO

acqR :: G -> IO R
acqR _ = (,) <$> newEmptyTMVarIO <*> newEmptyTMVarIO

relG :: G -> IO ()
relG _ = return ()

relR :: G -> R -> IO ()
relR _ _ = return ()

h :: Handler (G, R, (TBQueue Packet, TQueue Packet)) ()
h = liftSTM $ \i -> handleRead i <|> handleDelayedReply i

handleRead :: (G, R, (TBQueue Packet, TQueue Packet)) -> STM ()
handleRead (interconnect, (w, p), (inQ, outQ)) = do
  (addr, n, warp) <- handleWarp inQ outQ
  (do putTMVar interconnect (warp, w)
      putTMVar p (addr, n)
    ) <|> (do
      (warp', box) <- takeTMVar interconnect
      putTMVar box warp
      sendWarp (addr, n) warp' outQ
    )

handleWarp :: TBQueue Packet -> TQueue Packet -> STM (Word8, Word32, Warp)
handleWarp inQ outQ = do
  (addr, n, d) <- handlePacket Entrance inQ outQ
  case decodeWarp d of
    Just warp -> return (addr, n, warp)
    Nothing -> retry

handleDelayedReply :: (G, R, (TBQueue Packet, TQueue Packet)) -> STM ()
handleDelayedReply (_interconnect, (w, p), (_inQ, outQ)) = do
  (addr, n) <- takeTMVar p
  warp <- takeTMVar w
  sendWarp (addr, n) warp outQ

sendWarp :: (Word8, Word32) -> Warp -> TQueue Packet -> STM ()
sendWarp (addr, n) warp outQ = do
  let d = encodeWarp warp
  writeTQueue outQ $ DataPacket addr Entrance n d

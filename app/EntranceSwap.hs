{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE PartialTypeSignatures #-}
module EntranceSwap where

import Data.Word

import Control.Concurrent.STM

import Control.Applicative

import Control.Lens

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

decodeWarp :: [Word8] -> Warp
decodeWarp = undefined

encodeWarp :: Warp -> [Word8]
encodeWarp = undefined

handleEntranceSwap :: Handler _ _ _ _
handleEntranceSwap = withResources acqG relG acqR relR $ h

type G = (TMVar (Warp, TMVar Warp))
type R = (TMVar Warp, TMVar (Word8, Word32))

acqG :: IO G
acqG = newEmptyTMVarIO

acqR :: IO R
acqR = (,) <$> newEmptyTMVarIO <*> newEmptyTMVarIO

relG :: G -> IO ()
relG _ = return ()

relR :: R -> IO ()
relR _ = return ()

h :: Handler () () (G, R, (TBQueue Packet, TQueue Packet)) ()
h = liftSTM $ \i -> handleRead i <|> handleDelayedReply i

handleRead (interconnect, (w, p), (inQ, outQ)) = do
  (addr, n, d) <- handlePacket Entrance inQ outQ
  let warp = decodeWarp d
  (do putTMVar interconnect (warp, w)
      putTMVar p (addr, n)
    ) <|> (do
      (warp', box) <- takeTMVar interconnect
      putTMVar box warp
      sendWarp (addr, n) warp' outQ
    )
      
handleDelayedReply (interconnect, (w, p), (inQ, outQ)) = do
  (addr, n) <- takeTMVar p
  warp <- takeTMVar w
  sendWarp (addr, n) warp outQ

sendWarp (addr, n) warp outQ = do
  let d = encodeWarp warp
  writeTQueue outQ $ DataPacket addr Entrance n d

--todo cleanup

{-
import System.IO
import System.Environment (lookupEnv)

import Control.Exception (bracket, bracket_)

import Text.Read (readMaybe)

import qualified Data.ByteString as BS

import Control.Concurrent.STM
import Control.Concurrent.Async

import Data.Bits
import Data.Word
import Data.Maybe

import Network.Simple.TCP (serve, send, recv, Socket())

import Control.Monad
import Control.Applicative

import Control.Lens


data RawPacket =
    EnteredWarp Word32
  | WarpSrcRoomAndLayer Word8 Word8 Word8
  | WarpSrcCoords Word16 Word16
  | WarpDstRoomAndLayer Word8 Word8 Word8
  | WarpDstX Word16
  | WarpDstY Word16
  | WarpTileType Word16
  | WarpAnimAndSpawn Word8 Word8
  | UnknownPacket Word8 Word32

$(makePrisms ''RawPacket)

decodePacket :: Word32 -> (RawPacket, Bool)
decodePacket b = 
  let isNACK = testBit b 31
      payload = b .&. 0xFFFFFF
      packetType = (b `shiftR` 24) .&. 0x7F
      packet = case packetType of
       10 -> EnteredWarp payload
       11 -> (\(a,r,l) -> WarpSrcRoomAndLayer a r l) $ split3 payload 8
       12 -> uncurry WarpSrcCoords $ split payload 12
       13 -> (\(a,r,l) -> WarpDstRoomAndLayer a r l) $ split3 payload 8
       14 -> WarpDstX $ toIntegralSized' $ payload .&. 0xFFFF
       15 -> WarpDstY $ toIntegralSized' $ payload .&. 0xFFFF
       16 -> WarpTileType $ toIntegralSized' $ payload .&. 0xFFFF
       17 -> uncurry WarpAnimAndSpawn $ split payload 8
       n -> UnknownPacket (toIntegralSized' n) payload
  in (packet, isNACK)

split :: (Bits a, Integral a) => Word32 -> Int -> (a, a)
split w size = (toIntegralSized' $ (w `shiftL` (32 - (2*size))) `shiftR` (32 - size), toIntegralSized' $ (w `shiftL` (32 - size)) `shiftR` (32 - size))
split3 :: (Bits a, Integral a) => Word32 -> Int -> (a, a, a)
split3 w size = (toIntegralSized' $ (w `shiftL` (32 - (3*size))) `shiftR` (32 - size), toIntegralSized' $ (w `shiftL` (32 - 2*size)) `shiftR` (32 - size), toIntegralSized' $ (w `shiftL` (32 - size)) `shiftR` (32 - size))

combine :: (Bits a, Integral a) => (a, a) -> Int -> Word32
combine (x,y) size = (toIntegralSized' x `shiftL` size) .|. toIntegralSized' y
combine3 :: (Bits a, Integral a) => (a, a, a) -> Int -> Word32
combine3 (x,y,z) size = (toIntegralSized' x `shiftL` (size * 2)) .|. (toIntegralSized' y `shiftL` size) .|. toIntegralSized' z

toIntegralSized' :: (Bits a', Integral a', Bits a, Integral a) => a' -> a
toIntegralSized' = fromJust . toIntegralSized

encodePacket :: RawPacket -> Word32
encodePacket p = (toIntegralSized' (packetHeader p) `shiftL` 24) .|. packetPayload p
packetHeader :: RawPacket -> Word8
packetHeader (EnteredWarp _) = 10
packetHeader (WarpSrcRoomAndLayer _ _ _) = 11
packetHeader (WarpSrcCoords _ _) = 12
packetHeader (WarpDstRoomAndLayer _ _ _) = 13
packetHeader (WarpDstX _) = 14
packetHeader (WarpDstY _) = 15
packetHeader (WarpTileType _) = 16
packetHeader (WarpAnimAndSpawn _ _) = 17
packetHeader (UnknownPacket a _) = a
packetPayload :: RawPacket -> Word32
packetPayload (EnteredWarp p) = p
packetPayload (WarpSrcRoomAndLayer a r l) = combine3 (a,r,l) 8
packetPayload (WarpSrcCoords a b) = combine (a,b) 12
packetPayload (WarpDstRoomAndLayer a r l) = combine3 (a,r,l) 8
packetPayload (WarpDstX p) = toIntegralSized' p
packetPayload (WarpDstY p) = toIntegralSized' p
packetPayload (WarpTileType p) = toIntegralSized' p
packetPayload (WarpAnimAndSpawn a s) = combine (a,s) 8
packetPayload (UnknownPacket _ p) = p

data Result =
    SecondToSwap
  | FirstToSwap (TMVar Warp)
  | BeginEndgame
handle :: Maybe Word32 -> Socket -> TBQueue (Warp, TMVar Warp) -> TQueue Warp -> TVar Integer -> IO ()
handle warpIDBeginEndgame socket interconnect output playerCounter = do
  incomingPackets <- atomically newTQueue
  outgoingPackets <- atomically newTQueue
  withAsync (sender socket outgoingPackets) $ \_ -> withAsync (forever $ do 
    w <- atomically $ do
      warp <- readWarp incomingPackets
      c <- readTVar playerCounter
      writeTQueue output warp
      if (Just (warp ^. warpID) == warpIDBeginEndgame)
      then writeWarp outgoingPackets warp >> return BeginEndgame
      else handleSwap interconnect outgoingPackets warp
    case w of
      SecondToSwap -> return ()
      FirstToSwap deferredAnswer -> handleDeferredAnswer deferredAnswer outgoingPackets
      BeginEndgame -> bracket_ (debug "player entered endgame" >> atomically (modifyTVar' playerCounter (+ (-1)))) (debug "reincrementing player counter for leaving endgame" >> atomically (modifyTVar' playerCounter (+1))) $ reflectWarps incomingPackets output outgoingPackets
   ) $ \_ -> receiver socket incomingPackets outgoingPackets

reflectWarps :: TQueue RawPacket -> TQueue Warp -> TQueue RawPacket -> IO a
reflectWarps incoming output outgoing = forever $ atomically $ do
  w <- readWarp incoming
  writeTQueue output w
  writeWarp outgoing w

reflectIfSinglePlayer :: TVar Integer -> TBQueue (Warp, TMVar Warp) -> IO ()
reflectIfSinglePlayer c q = forever $ atomically $ do
  readTVar c >>= check . (== 1)
  (w, v) <- readTBQueue q
  putTMVar v w

readWarp :: TQueue RawPacket -> STM Warp
readWarp incomingPackets =
  expect incomingPackets (^? _EnteredWarp) $ \warpID ->
  expect incomingPackets (^? _WarpSrcRoomAndLayer) $ \(srcArea, srcRoom, srcLayer) -> 
  expect incomingPackets (^? _WarpSrcCoords) $ \(srcX, srcY) ->
  expect incomingPackets (^? _WarpDstRoomAndLayer) $ \(dstArea, dstRoom, dstLayer) -> 
  expect incomingPackets (^? _WarpDstX) $ \dstX ->
  expect incomingPackets (^? _WarpDstY) $ \dstY ->
  expect incomingPackets (^? _WarpTileType) $ \tileType ->
  expect incomingPackets (^? _WarpAnimAndSpawn) $ \(anim, spawn) ->
  return $ Warp warpID (Position srcArea srcRoom srcLayer srcX srcY) (Position dstArea dstRoom dstLayer dstX dstY) tileType anim spawn

handleDeferredAnswer :: TMVar Warp -> TQueue RawPacket -> IO ()
handleDeferredAnswer deferredAnswer outgoing = atomically $ do
  w <- takeTMVar deferredAnswer
  writeWarp outgoing w

writeWarp outgoing w = do
  writeTQueue outgoing $ WarpDstRoomAndLayer (w ^. warpNewPosition . positionArea) (w ^. warpNewPosition . positionRoom) (w ^. warpNewPosition . positionLayer)
  writeTQueue outgoing $ WarpDstX (w ^. warpNewPosition . positionX)
  writeTQueue outgoing $ WarpDstY (w ^. warpNewPosition . positionY)
  writeTQueue outgoing $ WarpTileType (w ^. warpTileType)
  writeTQueue outgoing $ WarpAnimAndSpawn (w ^. warpAnim) (w ^. warpSpawnState)

logOutput output = forever $ do
  w <- atomically $ readTQueue output
  print w

handleSwap interconnect outgoing warp = handleFirst interconnect warp <|> handleSecond interconnect outgoing warp
handleFirst interconnect warp = do
  v <- newEmptyTMVar
  writeTBQueue interconnect (warp, v)
  return $ FirstToSwap v
handleSecond interconnect outgoing warp = do
  (warp', v) <- readTBQueue interconnect
  putTMVar v warp
  writeWarp outgoing warp'
  return SecondToSwap

expect :: TQueue c -> (c -> Maybe a) -> (a -> STM b) -> STM b
expect input filter action = expect' input [] filter action where
  expect' input rejected filter action = do
    candidate <- readTQueue input
    case filter candidate of
      Nothing -> expect' input (candidate : rejected) filter action
      Just accepted -> do
        forM_ (reverse rejected) $ writeTQueue input
        action accepted

bytes :: (FiniteBits a, Integral a) => a -> BS.ByteString
bytes a = BS.pack $ go (finiteBitSize a `quot` 8) [] a where
  go :: (Bits a, Integral a) => Int -> [Word8] -> a -> [Word8]
  go 0 r _ = reverse r
  go n r a = go (n - 1) (toIntegralSized' (a .&. 0xFF) : r) (a `shiftR` 8)

sender sock outgoingPackets = forever $ do
  p <- atomically $ readTQueue outgoingPackets
  send sock $ bytes $ encodePacket p

receiver sock incoming outgoing = do
  bytes <- atomically $ newTBQueue 4
  withAsync (decodeFromBytes bytes incoming outgoing) $ \_ -> readBytes sock bytes

readBytes sock bytes = recv sock 4 >>= \case
  Nothing -> return ()
  Just bs -> do
    forM_ (BS.unpack bs) $ atomically . writeTBQueue bytes
    readBytes sock bytes

decodeFromBytes bytes incoming outgoing = forever $ atomically $ do
  a <- toIntegralSized' <$> readTBQueue bytes
  b <- toIntegralSized' <$> readTBQueue bytes
  c <- toIntegralSized' <$> readTBQueue bytes
  d <- toIntegralSized' <$> readTBQueue bytes
  let (p, isNack) = decodePacket (a .|. (b `shiftL` 8) .|. (c `shiftL` 16) .|. (d `shiftL` 24))
  if isNack then writeTQueue outgoing p else writeTQueue incoming p
-}

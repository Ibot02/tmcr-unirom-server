{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Protocol where

import Data.Bits
import Data.Word

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Control.Concurrent.STM

import Control.Lens

import Data.Conduit
import qualified Data.Conduit.Combinators as C

data Packet =
  DataPacket { packetAddr :: Word8
             , packetPort :: Port
             , packetNum  :: Word32
             , packetData :: [Word8]
             }
  | AckPacket { packetPort :: Port
              , packetNum  :: Word32
              }
  deriving (Eq, Ord, Show)

data Port = Peek | Pickup | SendItem | Entrance
          deriving (Eq, Ord, Show, Enum)

$(makePrisms ''Packet)

awaitW32 :: (Monad m) => ConduitT Word8 o m (Maybe Word32)
awaitW32 = do
  a <- await
  b <- await
  c <- await
  d <- await
  return $ decodeLE <$> a <*> b <*> c <*> d

decodeLE :: Word8 -> Word8 -> Word8 -> Word8 -> Word32
decodeLE (fromIntegral -> a) (fromIntegral -> b) (fromIntegral -> c) (fromIntegral -> d) = a .|. (b `shiftL` 8) .|. (c `shiftL` 16) .|. (d `shiftL` 24)

yieldW32 :: (Monad m) => Word32 -> ConduitT i Word8 m ()
yieldW32 (encodeLE -> (a,b,c,d)) = do
  yield a
  yield b
  yield c
  yield d

encodeLE :: Word32 -> (Word8, Word8, Word8, Word8) 
encodeLE w = (fromIntegral a, fromIntegral b, fromIntegral c, fromIntegral d) where
  a = w .&. 0xFF
  b = (w `shiftR` 8) .&. 0xFF
  c = (w `shiftR` 16) .&. 0xFF
  d = (w `shiftR` 32) .&. 0xFF

readPackets :: (Monad m) => ConduitT ByteString Packet m ()
readPackets = C.concat .| awaitForever readPacket

readPacket :: (Monad m) => Word8 -> ConduitT Word8 Packet m ()
readPacket 0x41 = do --ack
    p <- fmap (toEnum . fromIntegral) <$> await
    n <- awaitW32
    case AckPacket <$> p <*> n of
      Nothing -> return ()
      Just packet -> yield packet
readPacket 0x44 = do --data
    addr <- await
    p <- fmap (toEnum . fromIntegral) <$> await
    n <- awaitW32
    l <- await
    case (,,,) <$> addr <*> p <*> n <*> l of
      Nothing -> return ()
      Just (addr', p', n', l') -> do
        d <- C.take (fromIntegral l') .| C.sinkList
        yield $ DataPacket addr' p' n' d
readPacket _ = return ()

writePackets :: (Monad m) => ConduitT Packet ByteString m ()
writePackets = awaitForever writePacketChunk

writePacketChunk :: (Monad m) => Packet -> ConduitT i ByteString m ()
writePacketChunk p = do
  x <- writePacket p .| C.sinkList
  yield $ BS.pack x

writePacket :: (Monad m) => Packet -> ConduitT i Word8 m ()
writePacket (DataPacket a p n d) = do
  yield 0x44
  yield a
  yield $ fromIntegral $ fromEnum p
  yieldW32 n
  yield $ fromIntegral $ length d
  C.yieldMany d
writePacket (AckPacket p n) = do
  yield 0x41
  yield $ fromIntegral $ fromEnum p
  yieldW32 n


handlePacket :: Port -> TBQueue Packet -> TQueue Packet -> STM (Word8, Word32, [Word8])
handlePacket channel inQ outQ = do
  packet <- readTBQueue inQ
  case packet of
    (DataPacket addr channel' n d) -> do
      check (channel == channel')
      writeTQueue outQ $ AckPacket channel n
      return (addr, n, d)
    _ -> retry

handleAck :: (Word32 -> STM a) -> Port -> TBQueue Packet -> STM a
handleAck h p' inQ = do
  packet <- readTBQueue inQ
  case packet of
    AckPacket p n -> do
      check $ p == p'
      h n
    _ -> retry

ignoreAck :: Port -> TBQueue Packet -> STM ()
ignoreAck = handleAck (const (return ()))

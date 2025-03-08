{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
module Protocol (Packet(..), Port(..), handlePacket, handleAck, ignoreAck, readPackets, writePackets) where

import Data.Word

import Data.Binary.Get
import Data.Binary.Put

import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

import Control.Concurrent.STM

import Control.Lens

import Control.Monad.Catch (MonadThrow)

import Data.Conduit
import Data.Conduit.Serialization.Binary

data Packet =
  DataPacket { packetAddr :: Word8
             , packetPort :: Port
             , packetNum  :: Word32
             , packetData :: ByteString
             }
  | AckPacket { packetPort :: Port
              , packetNum  :: Word32
              }
  deriving (Eq, Ord, Show)

data Port = Peek | Pickup | SendItem | Entrance
          deriving (Eq, Ord, Show, Enum)

$(makePrisms ''Packet)

readPackets :: (MonadThrow m) => ConduitT ByteString Packet m ()
readPackets = conduitGet getPacket
writePackets :: (Monad m) => ConduitT Packet ByteString m ()
writePackets = awaitForever (return . putPacket) .| conduitPut

getPacket :: Get Packet
getPacket = do
  b <- getWord8
  case b of
    0x41 -> do
      port <- getPort
      n <- getWord32le
      return $ AckPacket port n
    0x44 -> do
      addr <- getWord8
      port <- getPort
      n <- getWord32le
      l <- getWord8
      d <- getByteString $ fromIntegral l
      return $ DataPacket addr port n d
    _ -> fail "unexpected header"

getPort :: Get Port
getPort = getWord8 >>= return . toEnum . fromIntegral
 
putPacket :: Packet -> Put
putPacket (AckPacket p n) = do
  putWord8 0x41
  putPort p
  putWord32le n
putPacket (DataPacket addr p n d) = do
  putWord8 0x44
  putWord8 addr
  putPort p
  putWord32le n
  putWord8 $ fromIntegral $ BS.length d
  putByteString d

putPort :: Port -> Put
putPort = putWord8 . fromIntegral . fromEnum

handlePacket :: Port -> TBQueue Packet -> TQueue Packet -> STM (Word8, Word32, ByteString)
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

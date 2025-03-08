{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
module Items where

import Handler
import Protocol

import System.Environment
import Data.Word
import Data.Bits
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import Control.Concurrent.STM
import Control.Concurrent.STM.TSem
import qualified StmContainers.Map as TM

import Control.Applicative

import qualified Data.ByteString.Lazy as BS

import Data.Aeson (throwDecode, FromJSON(..), genericParseJSON, Options(..), camelTo2, defaultOptions)

import GHC.Generics (Generic)

type WorldID = Word8
type Flag = Word16
type Item = Word16

data Placement = Placement {
    _placementWorld :: WorldID
  , _placementItem :: Item
  } deriving (Eq, Ord, Show, Generic)

aesonOptions :: Options
aesonOptions = defaultOptions {fieldLabelModifier = dropWhile (== '_') . dropWhile (/= '_') . dropWhile (== '_') . camelTo2 '_', omitNothingFields = True}

instance FromJSON Placement where
  parseJSON = genericParseJSON aesonOptions

type Placements = Map WorldID (Map Flag Placement)

-- Map Word (Map Flag [(World, Item)])

itemsHandler :: FilePath -> Handler _ _ (TBQueue Packet, TQueue Packet) ()
itemsHandler f = withResources (acqG f) relG acqR relR $ h

type G = (Placements, TM.Map Word8 (TQueue Item))
type R = (TMVar Word8, TSem)

acqG :: FilePath -> IO G
acqG f = (,) <$> (BS.readFile f >>= throwDecode) <*> TM.newIO

acqR :: IO R
acqR = (,) <$> newEmptyTMVarIO <*> (atomically $ newTSem 1)

relG :: G -> IO ()
relG _ = return ()

relR :: R -> IO ()
relR _ = return ()

h :: Handler () () (G, R, (TBQueue Packet, TQueue Packet)) ()
h = liftSTM $ \i -> handleOutbound i <|> handleLogin i <|> handlePeek i <|> handleLookup i

handleLogin ((_, _),(whoAmI, _),(inQ, outQ)) = do
  (addr, n, d) <- handlePacket SendItem inQ outQ
  case d of
    [w] -> do
      writeTMVar whoAmI w
    _ -> return ()
handlePeek ((placements, _), (whoAmI, _), (inQ, outQ)) = ignoreAck Peek inQ <|> do
  (addr, n, d) <- handlePacket Peek inQ outQ
  case d of
    [b, b'] -> do
      w <- readTMVar whoAmI
      let (_, item) = findItem placements w (b, b')
      sendItem addr Peek n item outQ
    _ -> return ()
handleLookup ((placements, items), (whoAmI, _), (inQ, outQ)) = ignoreAck Pickup inQ <|> do
  (addr, n, d) <- handlePacket Pickup inQ outQ
  case d of
    [b, b'] -> do
      w <- readTMVar whoAmI
      let (world, item) = findItem placements w (b, b')
      if world == w then
        sendItem addr Pickup n item outQ
      else do
        sendItem addr Pickup n 63 outQ
        queueItem items world item
    _ -> return ()

sendItem :: Word8 -> Port -> Word32 -> Word16 -> TQueue Packet -> STM ()
sendItem addr p n i q = writeTQueue q $ DataPacket addr p n $ (\(b,b') -> [b,b']) $ encodeLE16 i

findItem :: Placements -> WorldID -> (Word8, Word8) -> (WorldID, Item)
findItem placements world (decodeLE16 -> flag) = fromMaybe (world, 0xFFFF) $ do
  w <- M.lookup world placements
  Placement w' i <- M.lookup flag w
  return (w', i)

encodeLE16 :: Word16 -> (Word8, Word8)
encodeLE16 w = (fromIntegral (w .&. 0xFF), fromIntegral (w `shiftR` 8))
decodeLE16 :: (Word8, Word8) -> Word16
decodeLE16 (b, b') = fromIntegral b .|. (fromIntegral b' `shiftL` 8)

queueItem :: TM.Map WorldID (TQueue Item) -> WorldID -> Item -> STM ()
queueItem q w i = do
  c <- TM.lookup w q
  c' <- case c of
    Nothing -> do
      c' <- newTQueue
      TM.insert c' w q
      return c'
    Just c' -> return c'
  writeTQueue c' i

ignoreAck = handleAck (const (return ()))

handleOutbound ((_, items),(whoAmI, lock),(inQ, outQ)) = sendOutbound <|> handleAck' where
  sendOutbound = do
    w <- readTMVar whoAmI
    waitTSem lock
    c <- TM.lookup w items
    i <- case c of
      Nothing -> retry
      Just c' -> readTQueue c'
    sendItem' i
  handleAck' = handleAck (const $ signalTSem lock) SendItem inQ
  sendItem' i = sendItem 0 SendItem 0 i outQ

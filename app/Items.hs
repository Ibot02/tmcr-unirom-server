{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE ViewPatterns #-}
module Items where

import Handler
import Protocol

import Data.Word
import Data.Bits
import Data.Maybe

import Data.Map (Map)
import qualified Data.Map as M

import Control.Concurrent.STM
import qualified StmContainers.Map as TM

import Control.Applicative
import Control.Monad

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

itemsHandler :: FilePath -> Handler (TBQueue Packet, TQueue Packet) ()
itemsHandler f = withResources (acqG f) relG acqR relR $ h

type G = (Placements, TM.Map Word8 (TQueue Item, TQueue Item))
type R = (TMVar Word8, TMVar Item)

acqG :: FilePath -> IO G
acqG f = (,) <$> (BS.readFile f >>= throwDecode) <*> TM.newIO

acqR :: G -> IO R
acqR _ = (,) <$> newEmptyTMVarIO <*> newEmptyTMVarIO

relG :: G -> IO ()
relG _ = return ()

relR :: G -> R -> IO ()
relR (_, queues) (whoAmI, lastSent) = atomically $ (isEmptyTMVar lastSent >>= check) <|> do
  i <- takeTMVar lastSent
  w <- readTMVar whoAmI
  q <- getQueue queues w
  unGetTQueue q i

h :: Handler (G, R, (TBQueue Packet, TQueue Packet)) ()
h = liftSTM $ \i -> handleOutbound i <|> handleLoginOrSave i <|> handlePeek i <|> handleLookup i

handleLoginOrSave :: (G, R, (TBQueue Packet, TQueue Packet)) -> STM ()
handleLoginOrSave ((_, qs),(whoAmI, _),(inQ, outQ)) = do
  (_, _, d) <- handlePacket SendItem inQ outQ
  handleLogin whoAmI d qs <|> handleSave whoAmI qs

handleLogin :: TMVar Word8 -> [Word8] -> TM.Map Word8 (TQueue Item, TQueue Item) -> STM ()
handleLogin whoAmI [w] qs = do
  writeTMVar whoAmI w
  q' <- TM.lookup w qs
  case q' of
    Nothing -> return ()
    Just (q, floating) -> do
      items <- flushTQueue floating
      forM_ items $ \item -> writeTQueue q item >> writeTQueue floating item
handleLogin _ _ _ = error "unexpected login message"

handleSave :: TMVar Word8 -> TM.Map Word8 (TQueue Item, TQueue Item) -> STM ()
handleSave whoAmI qs = do
  (_, floating) <- readTMVar whoAmI >>= getQueues qs
  _ <- flushTQueue floating
  return ()

handlePeek :: (G, R, (TBQueue Packet, TQueue Packet)) -> STM ()
handlePeek ((placements, _), (whoAmI, _), (inQ, outQ)) = ignoreAck Peek inQ <|> do
  (addr, n, d) <- handlePacket Peek inQ outQ
  case d of
    [b, b'] -> do
      w <- readTMVar whoAmI
      let (_, item) = findItem placements w (b, b')
      sendItem addr Peek n item outQ
    _ -> return ()

handleLookup :: (G, R, (TBQueue Packet, TQueue Packet)) -> STM ()
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

getQueue ::  TM.Map WorldID (TQueue Item, TQueue Item) -> WorldID -> STM (TQueue Item)
getQueue qs w = fmap fst $ getQueues qs w
getQueues ::  TM.Map WorldID (TQueue Item, TQueue Item) -> WorldID -> STM (TQueue Item, TQueue Item)
getQueues qs w = do
  c <- TM.lookup w qs
  case c of
    Nothing -> do
      c' <- newTQueue
      c'' <- newTQueue
      TM.insert (c', c'') w qs
      return (c', c'')
    Just c' -> return c'

queueItem :: TM.Map WorldID (TQueue Item, TQueue Item) -> WorldID -> Item -> STM ()
queueItem qs w i = do
  (q, q') <- getQueues qs w
  writeTQueue q i
  writeTQueue q' i

handleOutbound :: (G, R, (TBQueue Packet, TQueue Packet)) -> STM ()
handleOutbound ((_, items),(whoAmI, lastSent),(inQ, outQ)) = sendOutbound <|> handleAck' where
  sendOutbound = do
    w <- readTMVar whoAmI
    c <- TM.lookup w items
    i <- case c of
      Nothing -> retry
      Just (c', _) -> readTQueue c'
    putTMVar lastSent i
    sendItem' i
  handleAck' = handleAck (const $ takeTMVar lastSent >> return ()) SendItem inQ
  sendItem' i = sendItem 0 SendItem 0 i outQ

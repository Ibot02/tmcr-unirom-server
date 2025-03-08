{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
module Main where

import System.IO

import Control.Exception (finally)

import Control.Concurrent.STM
import Control.Concurrent.Async

import Data.Conduit
import Data.Conduit.Network

import Control.Monad
import Control.Monad.IO.Class

import Protocol (Packet(..), readPackets, writePackets)
import Handler
import Settings
import Items

debug :: String -> IO ()
debug = hPutStrLn stderr

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering
  hSetBuffering stderr LineBuffering
  s <- envSettings
  runServer (serverSettings (port s) (hostPref s)) $ mainHandler s

runServer :: ServerSettings -> Handler g r (TBQueue Packet, TQueue Packet) () -> IO ()
runServer settings handler =
  global_prepare handler $ \g ->
    flip finally (cleanup_global handler g) $
    runTCPServer settings $ \app -> prepare handler $ \r -> finally (cleanup handler r) $ do
      inQ <- newTBQueueIO 20
      outQ <- newTQueueIO
      withAsync (forever $ atomically $ handle handler g r (inQ, outQ)) $ \_ ->
       withAsync (runConduit $ queueSource outQ .| writePackets .| appSink app) $ \_ ->
       runConduit $ appSource app .| readPackets .| queueSink inQ --this one isn't async so we terminate if remote closes the connection

queueSink :: (MonadIO m, Show i) => TBQueue i -> ConduitT i o m ()
queueSink queue = awaitForever $ \p -> do
  liftIO $ debug $ "< " <> show p
  liftIO $ atomically $ writeTBQueue queue p

queueSource :: (MonadIO m, Show o) => TQueue o -> ConduitT i o m ()
queueSource queue = forever $ do
  p <- liftIO $ atomically $ readTQueue queue
  liftIO $ debug $ "> " <> show p
  yield p

mainHandler :: AppSettings -> Handler _ _ (TBQueue Packet, TQueue Packet) ()
mainHandler s = itemsHandler (placementsFilepath s)

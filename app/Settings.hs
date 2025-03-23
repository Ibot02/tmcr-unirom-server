{-# LANGUAGE OverloadedStrings #-}
module Settings where

import System.Environment (lookupEnv)
import Data.Conduit.Network (HostPreference)
import Data.String (IsString(..))

data AppSettings = AppSettings {
    hostPref :: HostPreference
  , port :: Int
  , placementsFilepath :: String
  , entranceLogFilepath :: Maybe String
  , swapEntrances :: Bool
  } deriving (Eq, Ord, Show)


envSettings :: IO AppSettings
envSettings = AppSettings
  <$> env' "HOSTNAME" fromString "*"
  <*> env' "PORT" read 8000
  <*> env' "PLACEMENTS_FILEPATH" id "/placements.json"
  <*> lookupEnv "ENTRANCE_LOG_FILEPATH"
  <*> env' "ENTRANCE_SWAP" (const True) False

env' :: String -> (String -> a) -> a -> IO a
env' e c d = maybe d c <$> lookupEnv e

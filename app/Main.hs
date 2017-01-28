{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO

import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger

import           Network.HTTP.Client                  (newManager)
import           Network.HTTP.Client.TLS              (tlsManagerSettings)

import           Control.Monad.Trans.Maybe

import           App
import           Configuration
import           HmacMiddleware
import           Lib

-- ----------------------------------------------

-- TODO: pimp error handling (Maybe -> Either/Except?)
main :: IO ()
main = do
  mbConfig <- runMaybeT loadConfiguration
  case mbConfig of
    Just config -> do
      let optAuthware = case cfgGithubSecret config of
            Just secret -> hmacAuth $ defaultAuthSettings secret
            Nothing     -> id
      let middleware = logStdoutDev . optAuthware
      manager        <- newManager tlsManagerSettings
      let context    = AppContext config manager
      run (cfgPort config) $ middleware $ app context
    Nothing -> hPutStrLn stderr "Incomplete/invalid configuration" -- I'm just a Maybe, baby


loadConfiguration :: MaybeT IO Configuration
loadConfiguration =
  Configuration
    <$> envRead    "PORT"
    <*> envMaybeBS "GITHUB_SECRET"
    <*> env        "MATTERMOST_URL"
    <*> envRead    "MATTERMOST_PORT"
    <*> env        "MATTERMOST_API_KEY"

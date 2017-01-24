{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger

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
      run (cfgPort config) $ middleware $ app config
      startApp config
    Nothing -> putStrLn "Incomplete/invalid configuration" -- I'm just a Maybe, baby


loadConfiguration :: MaybeT IO Configuration
loadConfiguration =
  Configuration
    <$> envRead    "PORT"
    <*> envMaybeBS "GITHUB_SECRET"
    <*> env        "MATTERMOST_URL"
    <*> envRead    "MATTERMOST_PORT"
    <*> env        "MATTERMOST_API_KEY"

{-# LANGUAGE OverloadedStrings #-}

module Main where

import           System.IO
import           System.Log.Formatter
import           System.Log.Handler
import           System.Log.Handler.Simple
import           System.Log.Logger                    hiding (debugM, errorM,
                                                       warningM)
import qualified System.Log.Logger                    as Log

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
  initLoggers DEBUG
  mbConfig <- runMaybeT loadConfiguration
  case mbConfig of
    Just config -> do
      let optAuthware = case cfgGithubSecret config of
            Just secret -> hmacAuth $ defaultAuthSettings secret
            Nothing     -> id
      let middleware = logStdoutDev . optAuthware
      manager        <- newManager tlsManagerSettings
      let context    = AppContext config manager
      putStrLn $ "Starting ghmm (port " ++ (show . cfgPort $ config) ++ ")"
      run (cfgPort config) $ middleware $ app context
    Nothing ->
      errorM "Incomplete/invalid configuration"


loadConfiguration :: MaybeT IO Configuration
loadConfiguration =
  Configuration
    <$> envRead    "PORT"
    <*> envRead    "LOGLEVEL"             `withDef` Log.ERROR
    <*> envMaybeBS "GITHUB_SECRET"
    <*> env        "MATTERMOST_URL"
    <*> envRead    "MATTERMOST_PORT"
    <*> env        "MATTERMOST_API_KEY"


initLoggers :: Priority -> IO ()
initLoggers prio = do
  let defFormatter = simpleLogFormatter "[$time : $loggername : $prio] $msg"
  -- root does not have a priority
  updateGlobalLogger rootLoggerName clearLevel
  -- stdout root logger
  handlerBare <- streamHandler stdout prio `withFormatter` defFormatter
  updateGlobalLogger rootLoggerName (setHandlers [handlerBare])


withFormatter :: (Monad m, LogHandler r) => m r -> LogFormatter r -> m r
withFormatter h f = fmap (`setFormatter` f) h

-- ----------------------------------------------

modName :: String
modName = "Main"

errorM :: String -> IO ()
errorM = Log.errorM modName

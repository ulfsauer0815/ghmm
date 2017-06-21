{-# LANGUAGE OverloadedStrings #-}

-- | The server listening for GitHub events and relaying them to Mattermost.
module Main where

import           System.Environment
import           System.IO
import           System.Log
import           System.Log.Handler.Simple
import           System.Log.Logger                    hiding (debugM, errorM,
                                                       warningM)
import qualified System.Log.Logger                    as Log

import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger

import           Network.HTTP.Client                  (newManager)
import           Network.HTTP.Client.TLS              (tlsManagerSettings)

import           App
import           Config
import           HmacMiddleware
import           Lib
import           LogFormatter

-- ----------------------------------------------

-- TODO: pimp error handling (Maybe -> Either/Except?)
-- | Starts the server.
main :: IO ()
main = do
  args <- getArgs
  initLoggers DEBUG

  mbConfig <- loadConfig $ headMaybe args
  case mbConfig  of
    Right config -> do
      let optAuthware = case cfgGithubSecret config of
            Just secret -> hmacAuth $ defaultAuthSettings secret
            Nothing     -> id
      let middleware = logStdoutDev . optAuthware
      manager        <- newManager tlsManagerSettings
      let context    = AppContext config manager
      putStrLn $ "Starting ghmm (port " ++ (show . cfgPort $ config) ++ ")"
      run (cfgPort config) $ middleware $ app context
    Left e ->
      errorM e
  where
  headMaybe xs = if null xs then Nothing else Just $ head xs


-- | Initialize the loggers with the log level.
initLoggers :: Priority -> IO ()
initLoggers prio = do
  -- root does not have a priority
  updateGlobalLogger rootLoggerName clearLevel

  -- stdout root logger
  logFormatter <- getSimpleLogFormatter stdout
  handlerBare <- streamHandler stdout prio `withFormatter` logFormatter logFormat
  updateGlobalLogger rootLoggerName (setHandlers [handlerBare])


-- | Log output format.
logFormat :: String
logFormat = "[$time : $loggername : $prio] $msg"

-- ----------------------------------------------

modName :: String
modName = "Main"

errorM :: String -> IO ()
errorM = Log.errorM modName

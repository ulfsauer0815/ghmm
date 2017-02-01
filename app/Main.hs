{-# LANGUAGE OverloadedStrings #-}

module Main where

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
import           Configuration
import           HmacMiddleware
import           Lib
import           LogFormatter

-- ----------------------------------------------

-- TODO: pimp error handling (Maybe -> Either/Except?)
main :: IO ()
main = do
  initLoggers DEBUG
  mbConfig <- runConfigReader readConfig
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


readConfig :: ConfigReader Configuration
readConfig =
  Configuration
    <$> envRead       "PORT"                 `withDef` 8000
    <*> envRead       "LOG_LEVEL"            `withDef` Log.ERROR
    <*> envBS   `opt` "GITHUB_SECRET"        `withDef` Nothing
    <*> env           "MATTERMOST_URL"
    <*> envRead       "MATTERMOST_PORT"      `withDef` 443
    <*> env           "MATTERMOST_API_KEY"


initLoggers :: Priority -> IO ()
initLoggers prio = do
  -- root does not have a priority
  updateGlobalLogger rootLoggerName clearLevel

  -- stdout root logger
  formatter <- getSimpleLogFormatter stdout
  handlerBare <- streamHandler stdout prio `withFormatter` formatter logFormat
  updateGlobalLogger rootLoggerName (setHandlers [handlerBare])


logFormat :: String
logFormat = "[$time : $loggername : $prio] $msg"

-- ----------------------------------------------

modName :: String
modName = "Main"

errorM :: String -> IO ()
errorM = Log.errorM modName

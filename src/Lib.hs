{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( startApp
    , app
    , Configuration(..)
    ) where

import           Control.Monad.Except     (ExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Reader     (runReaderT)


import           Data.Aeson
import           Data.Monoid
import           Data.Text                (Text)

import qualified System.Log.Logger        as Log

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant

import           App
import           Github.Api               as Github
import           Github.Event.Json
import           Mattermost.Github

-- ----------------------------------------------

server :: ServerT Github.Api App
server = eventHandler

appToServer :: AppContext -> Server Github.Api
appToServer ctx = enter (convertApp ctx) server

convertApp :: AppContext -> App :~> ExceptT ServantErr IO
convertApp ctx = Nat (flip runReaderT ctx . runApp)

-- ----------------------------------------------

startApp :: AppContext -> IO ()
startApp ctx = run (cfgPort . ctxConfiguration $ ctx) (app ctx)

app :: AppContext -> Application
app ctx = serve (Proxy :: Proxy Github.Api) (appToServer ctx)

api :: Proxy Github.Api

api = Proxy
-- ----------------------------------------------

-- TODO: don't block
eventHandler :: Maybe Text -> Value -> App NoContent
eventHandler eventType jsonEvent = do
  liftIO $ do
    debugM $ "Handling GitHub event type: " <> show eventType
    debugM $ "Handling GitHub event: " <> show jsonEvent
  case decodeEvent eventType jsonEvent of
    Just e  -> do
      liftIO . debugM $ "Parsed GitHub event: " <> show e
      dispatch e
    Nothing -> do
      liftIO . warningM $ "Unable to parse GitHub event type: " <> show eventType
      return NoContent

dispatch :: Event -> App NoContent
dispatch e = do
  postEvent e
  return NoContent

-- ----------------------------------------------

modName :: String
modName = "Lib"

warningM :: String -> IO ()
warningM = Log.warningM modName

debugM :: String -> IO ()
debugM = Log.debugM modName

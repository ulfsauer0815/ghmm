{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( startApp
    , app
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
import           Github.Event.Filter
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
eventHandler :: Maybe Text -> Maybe Text-> Value -> App NoContent
eventHandler deliveryHeader eventHeader jsonEvent =
  case deliveryHeader of
    Nothing -> do
      liftIO $ warningM "Request without X-GitHub-Delivery header"
      throwError $ err400 { errBody = "X-GitHub-Delivery header missing" }
    Just deliveryId ->
      case eventHeader of
        Nothing -> do
          liftIO $ warningM "Request without X-Github-Event header"
          throwError $ err400 { errBody = "X-Github-Event header missing" }
        Just eventType ->
          eventHandler' deliveryId eventType jsonEvent

eventHandler' :: Text -> Text -> Value -> App NoContent
eventHandler' deliveryId eventType jsonEvent = do
  liftIO $ do
    debugM $ "Handling GitHub event type: " <> show eventType
    debugM $ "Handling GitHub event: " <> show jsonEvent
  case decodeEvent eventType jsonEvent of
    Right e  -> do
      liftIO . debugM $ "Parsed GitHub event: " <> show e
      handleEvent $ Event e eventType deliveryId
    Left msg -> do
      liftIO . warningM $
        "Unable to parse GitHub event type \""
          <> show eventType <> "\": "
          <> msg
      return NoContent


handleEvent :: Event -> App NoContent
handleEvent e =
  if isInterestingEvent e
  then postEvent e
  else do
    liftIO $ debugM "Discarding boring event"
    return NoContent

-- ----------------------------------------------

modName :: String
modName = "Lib"

warningM :: String -> IO ()
warningM = Log.warningM modName

debugM :: String -> IO ()
debugM = Log.debugM modName

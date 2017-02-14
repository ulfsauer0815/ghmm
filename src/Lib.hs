{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | The app listening for GitHub events and relaying them to Mattermost.
module Lib
    ( startApp
    , app
    ) where

import           Control.Monad.Except       (ExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Reader       (runReaderT)


import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as BS
import           Data.Monoid
import           Data.Text                  (Text)

import qualified System.Log.Logger          as Log

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant

import           App
import           Github.Api                 as Github
import           Github.Event.Json
import           Github.Event.Predicate
import           Mattermost.Github.Client

-- ----------------------------------------------

server :: ServerT Github.Api App
server = eventHandler

appToServer :: AppContext -> Server Github.Api
appToServer ctx = enter (convertApp ctx) server

convertApp :: AppContext -> App :~> ExceptT ServantErr IO
convertApp ctx = Nat (flip runReaderT ctx . runApp)

-- ----------------------------------------------

-- | Start the app with the given context.
startApp :: AppContext -> IO ()
startApp ctx = run (cfgPort . ctxConfiguration $ ctx) (app ctx)

-- | The app as an WAI 'Application' with the given context.
app :: AppContext -> Application
app ctx = serve (Proxy :: Proxy Github.Api) (appToServer ctx)

api :: Proxy Github.Api
api = Proxy

-- ----------------------------------------------

-- TODO: don't block
-- | The GitHub API 'Github.API' handler.
--   Handles incoming webhook calls and posts messages to Mattermost.
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


-- | The happy path of the 'eventHandler'.
--   Parses the incoming event using the header data and renders a message which
--   is sent to Mattermost.
eventHandler' :: Text -> Text -> Value -> App NoContent
eventHandler' deliveryId eventType jsonEvent = do
  liftIO . debugM $ "Handling GitHub event " <> show eventType <> ": "
                      <> (BS.unpack . encode $ jsonEvent)
  case decodeEvent eventType jsonEvent of
    Right e  -> do
      let event = Event e eventType deliveryId
      liftIO . debugM $ "Parsed GitHub event: " <> show event
      handleEvent event
    Left msg -> do
      liftIO . errorM $
        "Unable to parse GitHub event type "
          <> show eventType <> ": "
          <> msg
      return NoContent


-- | Handle an GitHub 'Event'..
--   Posts the event to Mattermost if it is interesting enough.
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

errorM :: String -> IO ()
errorM = Log.errorM modName

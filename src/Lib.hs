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
import           Data.Text                (Text)

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
    print eventType
    print jsonEvent
  case decodeEvent eventType jsonEvent of
    Just e  -> dispatch e
    Nothing -> do
      liftIO $ putStrLn "warn: unable to parse event"
      return NoContent

dispatch :: Event -> App NoContent
dispatch e = do
  liftIO $ print e
  postEvent e
  return NoContent

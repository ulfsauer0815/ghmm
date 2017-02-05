{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Mattermost.Github.Client
    ( Event(..)

    , postEvent
    ) where

import           Control.Monad.Reader
import           Control.Monad.Trans.Except (runExceptT)

import           Data.Monoid
import qualified Data.Text                  as T

import qualified System.Log.Logger          as Log

import           Servant
import           Servant.Client

import           App

import           Github.Event.Types

import           Mattermost.Api
import           Mattermost.Github.Message

-- ----------------------------------------------

postEvent :: Event -> App NoContent
postEvent e = do
  mmUrl             <- cfg cfgMattermostUrl
  mmPort            <- cfg cfgMattermostPort
  mmApiKey          <- cfg cfgMattermostApiKey
  httpClientManager <- asks ctxHttpClientManager
  res <- liftIO $ do
    debugM $ "Posting message to mattermost: " <> show message
    runExceptT $ hook mmApiKey
      message
      httpClientManager (BaseUrl Https (T.unpack mmUrl) mmPort "") -- TODO: hardcoded https?
  case res of
    Left err        -> liftIO . errorM $ "Unable to post to mattermost: " <> show err
    Right NoContent -> return ()
  return NoContent

  where
  message = renderMessage . evtPayload $ e

-- ----------------------------------------------

modName :: String
modName = "Mattermost.Github"

debugM :: String -> IO ()
debugM = Log.debugM modName

errorM :: String -> IO ()
errorM = Log.errorM modName

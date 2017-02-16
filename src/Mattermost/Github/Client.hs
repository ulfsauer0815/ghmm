{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | The Mattermost client.
module Mattermost.Github.Client
    ( Event(..)

    , postEvent
    ) where

import           Control.Monad.Reader

import           Data.Monoid

import qualified System.Log.Logger         as Log

import           Servant
import           Servant.Client

import           App

import           Github.Event.Types

import           Mattermost.Api
import           Mattermost.Github.Message

-- ----------------------------------------------

-- | Post an GitHub event to Mattermost.
postEvent :: Event -> App NoContent
postEvent e = do
  mmUrl             <- cfg cfgMattermostUrl
  mmApiKey          <- cfg cfgMattermostApiKey
  mmChannel         <- cfg cfgMattermostChannel
  httpClientManager <- asks ctxHttpClientManager
  let clientEnv = ClientEnv httpClientManager mmUrl
  res <- liftIO $ do
    let message = renderMessage' messageTemplate{ mptChannel = mmChannel } . evtPayload $ e
    debugM $ "Posting message to mattermost: " <> show message
    runClientM (hook mmApiKey message) clientEnv
  case res of
    Left err        -> liftIO . errorM $ "Unable to post to mattermost: " <> show err
    Right NoContent -> return ()
  return NoContent

-- ----------------------------------------------

modName :: String
modName = "Mattermost.Github"

debugM :: String -> IO ()
debugM = Log.debugM modName

errorM :: String -> IO ()
errorM = Log.errorM modName

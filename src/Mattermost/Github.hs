{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Mattermost.Github
    ( Event(..)
    
    , postEvent
    ) where

import           Control.Monad.Reader
import           Control.Monad.Trans.Except (runExceptT)

import           Data.Monoid
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

import           Network.HTTP.Client        (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)

import           Servant
import           Servant.Client

import           App

import           Github.Event.Types
import           Github.Event.Message

import           Mattermost.Api

-- ----------------------------------------------

postEvent :: Event -> App NoContent
postEvent e = do
  mmUrl    <- asks cfgMattermostUrl
  mmPort   <- asks cfgMattermostPort
  mmApiKey <- asks cfgMattermostApiKey
  -- TODO: put into context
  res <- liftIO $ do
    manager <- liftIO $ newManager tlsManagerSettings
    T.putStrLn $ "raw message: " <> messageText
    runExceptT $ hook mmApiKey
      payload
      manager (BaseUrl Https (T.unpack mmUrl) mmPort "") -- TODO: hardcoded https?
  case res of
    Left err        -> liftIO . putStrLn $ "Error: " <> show err
    Right NoContent -> return ()
  return NoContent

  where
  messageText = renderMessageText e
  payload = MessagePayload
    { text = messageText
    , username = Just "GitHub"
    , icon_url = Just "http://i.imgur.com/NQA4pPs.png"
    }

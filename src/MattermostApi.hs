{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module MattermostApi
    ( MattermostApi
    , postEvent
    ) where

import           GHC.Generics

import           Control.Monad.Reader
import           Control.Monad.Trans.Except (runExceptT)

import           Data.Aeson
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

import           Network.HTTP.Client        (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)

import           Servant
import           Servant.Client

import           App
import           EventMessageRendering
import           GithubApi

-- ----------------------------------------------

type MattermostApi =
       "hooks" :> Capture "key" Text :> ReqBody '[JSON] MessagePayload :> Post '[JSON] NoContent


{-# ANN type MessagePayload ("HLint: ignore Use camelCase" :: Text) #-}

data MessagePayload = MessagePayload
 { text     :: Text
 , username :: Maybe Text
 , icon_url :: Maybe Text
 } deriving (Eq, Show, Generic)

instance ToJSON MessagePayload

-- ----------------------------------------------

mattermostApi :: Proxy MattermostApi
mattermostApi = Proxy

-- hook :: Text -> MessagePayload -> Manager -> BaseUrl-> ClientM NoContent
hook :: Client MattermostApi
hook = client mattermostApi


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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Network.HTTP.Client        (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)

import           Control.Monad.Trans.Except (runExceptT)

import           Data.Text                  (Text)
import qualified Data.Text                  as T

import           Servant.Client

import           Configuration
import           Mattermost.Api

-- ----------------------------------------------

data Configuration = Configuration
  { cfgMattermostUrl     :: Text
  , cfgMattermostPort    :: Int
  , cfgMattermostApiKey  :: Text
  , cfgMattermostChannel :: Text
  }

-- ----------------------------------------------

main :: IO ()
main = do
  mbConfig <- runConfigReader readConfig
  case mbConfig of
    Just Configuration{..} -> do
      manager <- newManager tlsManagerSettings
      let payload = MessagePayload
            { text = "test message"
            , username = Just "GitHub Test"
            , icon_url = Just "http://i.imgur.com/fzz0wsH.jpg"
            , channel  = Just cfgMattermostChannel
            }
      runExceptT $ hook cfgMattermostApiKey
        payload
        manager (BaseUrl Https (T.unpack cfgMattermostUrl) cfgMattermostPort "")
      return ()
    Nothing ->
      putStrLn "Incomplete/invalid configuration"

-- ----------------------------------------------

readConfig :: ConfigReader Configuration
readConfig =
  Configuration
    <$> env           "MATTERMOST_URL"
    <*> envRead       "MATTERMOST_PORT"         `withDef` 443
    <*> env           "MATTERMOST_API_KEY"
    <*> env           "TEST_MATTERMOST_CHANNEL"

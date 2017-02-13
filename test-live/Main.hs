{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Main where

import           Network.HTTP.Client       (Manager, newManager)
import           Network.HTTP.Client.TLS   (tlsManagerSettings)

import           Control.Monad

import           Data.Aeson
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as BL
import           Data.Either
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           Servant.Client

import           Configuration
import           Github.Event.Json
import           Github.Event.Types
import           Mattermost.Api
import           Mattermost.Github.Message

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
    Just config -> do
      manager <- newManager tlsManagerSettings
      events  <- rights <$> forM eventData (uncurry loadEvent)
      forM_ events $ sendEvent manager config
    Nothing ->
      putStrLn "Incomplete/invalid configuration"

-- ----------------------------------------------

sendEvent :: Manager -> Configuration -> EventPayload -> IO ()
sendEvent clientManager Configuration{..} event = do
  let clientEnv = ClientEnv clientManager (BaseUrl Https (T.unpack cfgMattermostUrl) cfgMattermostPort "")
  let message   = renderMessage' testMessageTemplate event
  result <- runClientM (hook cfgMattermostApiKey message) clientEnv
  when (isLeft result) $ print result
  where
  testMessageTemplate = MessagePayload
    { mptText        = Nothing
    , mptUsername    = Just "GitHub Test"
    , mptIcon_url    = Just "http://i.imgur.com/fzz0wsH.jpg"
    , mptChannel     = Just cfgMattermostChannel
    , mptAttachments = []
    }


loadEvent :: String -> Text -> IO (Either String EventPayload)
loadEvent file header = do
  js <- loadFile file
  let valueEt = eitherDecode js :: Either String Value
  return $ valueEt >>= decodeEvent header


loadFile :: String -> IO ByteString
loadFile relPath = BL.readFile $ "test/data/event/" ++ relPath

-- ----------------------------------------------

readConfig :: ConfigReader Configuration
readConfig =
  Configuration
    <$> env           "MATTERMOST_URL"
    <*> envRead       "MATTERMOST_PORT"         `withDef` 443
    <*> env           "MATTERMOST_API_KEY"
    <*> env           "TEST_MATTERMOST_CHANNEL"

-- ----------------------------------------------

eventData :: [(String, Text)]
eventData =
  [ ("push.json",                         "push")
  , ("pullrequest.json",                  "pull_request")
  -- , ("status.json",                       "status")
  , ("status_with_description.json",      "status")
  , ("issues.json",                       "issues")
  , ("issuecomment.json",                 "issue_comment")
  , ("pullrequestreview.json",            "pull_request_review")
  , ("pullrequestreviewcomment.json",     "pull_request_review_comment")
  ]

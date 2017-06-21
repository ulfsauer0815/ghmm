{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | The Mattermost client.
module Mattermost.Github.Client
    ( Event(..)

    , postEvent
    ) where

import           Control.Applicative
import           Control.Monad.Reader

import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text                 as T

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
  let repoFullName = repFull_name . repository $ evtPayload e
  mmUrl             <- cfg cfgMattermostUrl
  mmApiKey          <- cfg cfgMattermostApiKey
  mmChannel         <- matchChannel repoFullName <$> cfg cfgRepositories
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

matchChannel :: Text -> Map Text RepositoryConfig -> Maybe Text
matchChannel repoFullName mapping =
  join $ rcgChannel <$> (M.lookup repoFullName mapping <|> M.lookup org mapping <|> M.lookup "_default" mapping)
  where
  (org:_repo:_) = T.split (=='/') repoFullName

-- ----------------------------------------------

modName :: String
modName = "Mattermost.Github"

debugM :: String -> IO ()
debugM = Log.debugM modName

errorM :: String -> IO ()
errorM = Log.errorM modName

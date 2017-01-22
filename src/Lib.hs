{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Lib
    ( startApp
    , app
    , Configuration(..)
    ) where

import           Debug.Trace

import           Control.Monad
import           Control.Monad.Except     (ExceptT)
import           Control.Monad.IO.Class
import           Control.Monad.Reader     (runReaderT)

import           Data.Aeson
import           Data.Aeson.Types         as AE
import           Data.Text                (Text)

import           Network.Wai
import           Network.Wai.Handler.Warp

import           Servant

import           App
import           GithubApi
import           MattermostApi

-- ----------------------------------------------

mainServer :: ServerT GithubApi App
mainServer = eventHandler

appToServer :: Configuration -> Server GithubApi
appToServer cfg = enter (convertApp cfg) mainServer

convertApp :: Configuration -> App :~> ExceptT ServantErr IO
convertApp cfg = Nat (flip runReaderT cfg . runApp)

-- ----------------------------------------------

startApp :: Configuration -> IO ()
startApp cfg = run 8000 (app cfg) -- TODO: put port into config

app :: Configuration -> Application
app cfg = serve (Proxy :: Proxy GithubApi) (appToServer cfg)

api :: Proxy GithubApi
api = Proxy

server :: ServerT GithubApi App
server = eventHandler


-- TODO: don't block
-- TODO: check secret
eventHandler :: Maybe Text -> Maybe Text -> Value -> App NoContent
eventHandler eventType hubSignature jsonEvent = do
  liftIO $ do
    print eventType
    print hubSignature
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

decodeEvent :: Maybe Text -> Value -> Maybe Event
decodeEvent eventType =
  parseMaybe (parseEvent eventType)

parseEvent :: Maybe Text -> Value -> Parser Event
parseEvent eventType v =
  case eventType of
    Just "push"          -> parsePushEvent v
    Just "pull_request"  -> parsePullRequestEvent v
    Just "status"        -> parseStatusEvent v
    Just "issue_comment" -> parseCommentEvent v
    Just et              ->
      trace ("warn: unhandled event type: " ++ show et) mzero -- FIXME: rm trace
    Nothing             -> mzero


parsePushEvent :: Value -> Parser Event
parsePushEvent (Object o) = PushEvent <$>
  o .: "ref"          <*>
  o .: "commits"      <*>
  o .: "head_commit"  <*>
  o .: "compare"      <*>
  o .: "repository"
parsePushEvent _ = mzero

parsePullRequestEvent :: Value -> Parser Event
parsePullRequestEvent (Object o) = PullRequestEvent <$>
  o .: "action"       <*>
  o .: "number"       <*>
  o .: "pull_request" <*>
  o .: "repository"
parsePullRequestEvent _ = mzero

parseStatusEvent :: Value -> Parser Event
parseStatusEvent (Object o) = StatusEvent <$>
  o .: "sha"          <*>
  o .: "state"        <*>
  o .: "description"  <*>
  o .: "target_url"   <*>
  o .: "repository"
parseStatusEvent _ = mzero

parseCommentEvent :: Value -> Parser Event
parseCommentEvent (Object o) = CommentEvent <$>
  o .: "action"       <*>
  o .: "issue"        <*>
  o .: "comment"      <*>
  o .: "repository"
parseCommentEvent _ = mzero

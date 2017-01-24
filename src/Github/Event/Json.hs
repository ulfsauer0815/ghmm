{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Github.Event.Json
    ( decodeEvent
    , parseEvent
    ) where

import           Debug.Trace

import           Control.Monad

import           Data.Aeson
import           Data.Aeson.Types         as AE
import           Data.Text                (Text)

import           Github.Event.Types

-- ----------------------------------------------

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

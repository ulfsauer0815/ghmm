{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Github.Event.Json
    ( decodeEvent
    ) where

import           Data.Aeson
import           Data.Map
import qualified Data.Map           as M
import           Data.Text          (Text)

import           Github.Event.Types

-- ----------------------------------------------

headerToConstructor :: Text -> Maybe Text
headerToConstructor = flip M.lookup mapping
  where
  mapping = fromList
    [ ("push",                        "PushEvent")
    , ("pull_request",                "PullRequestEvent")
    , ("status",                      "StatusEvent")
    , ("issue_comment",               "IssueCommentEvent")
    , ("pull_request_review",         "PullRequestReviewEvent")
    , ("pull_request_review_comment", "PullRequestReviewCommentEvent")
    ]


decodeEvent :: Text -> Value -> Either String Event
decodeEvent eventType v =
  case headerToConstructor eventType of
    Just constructor -> parseJSON' constructor v
    Nothing          -> fail "unknown event type"


injectHeader :: Text -> Value -> Value
injectHeader h o = object [h .= o]

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | JSON decoding/parsing functions for GitHub 'EventPayload's.

--   Deserializing GitHub events requires additional data supplied in the header
--   of the request. To parse an event, this metadata is injected into the
--   'Value' before parsing it.
module Github.Event.Json
    ( decodeEvent
    ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Map
import qualified Data.Map           as M
import           Data.Text          (Text)

import           Github.Event.Types

-- ----------------------------------------------

-- | The mapping from @X-GitHub-Event@ header to 'EventPayload' constructor.
headerToConstructor :: Text -> Maybe Text
headerToConstructor = flip M.lookup mapping
  where
  mapping = fromList
    [ ("ping",                        "PingEvent")
    , ("push",                        "PushEvent")
    , ("pull_request",                "PullRequestEvent")
    , ("status",                      "StatusEvent")
    , ("issues",                      "IssuesEvent")
    , ("issue_comment",               "IssueCommentEvent")
    , ("pull_request_review",         "PullRequestReviewEvent")
    , ("pull_request_review_comment", "PullRequestReviewCommentEvent")
    ]


-- | Decode a `Value` to a GitHub event payload using the @X-GitHub-Event@
--   header data as a hint for deserialization.
decodeEvent :: Text -> Value -> Either String EventPayload
decodeEvent eventType v =
  case headerToConstructor eventType of
    Just constructor -> parseJSON' constructor v
    Nothing          -> fail "unknown event type"


-- | \"Inject\" a value by wrapping it in an object with the given name.
injectConstructor :: Text -> Value -> Value
injectConstructor h o = object [h .= o]


-- | Parses a 'Value' by injecting the given value.
--   See 'injectConstructor'.
parseJSON' :: FromJSON a => Text -> Value -> Either String a
parseJSON' c = parseEither $ parseJSON . injectConstructor c

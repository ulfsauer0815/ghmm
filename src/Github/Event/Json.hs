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
    [ ("push",          "PushEvent")
    , ("pull_request",  "PullRequestEvent")
    , ("status",        "StatusEvent")
    , ("issue_comment", "CommentEvent")
    ]


decodeEvent :: Maybe Text -> Value -> Maybe Event
decodeEvent eventTypeMb v =
  case eventTypeMb >>= headerToConstructor of
    Just constructor -> parseJSON' constructor v
    Nothing          -> fail "unhandled event type or no type given"


injectHeader :: Text -> Value -> Value
injectHeader h o = object [h .= o]

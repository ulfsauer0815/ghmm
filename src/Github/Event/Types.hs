{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Github.Event.Types
    ( Event(..)
    , Commit(..)
    , Repository(..)
    , PullRequest(..)
    , Issue(..)
    , Comment(..)
    , User(..)

    , parseJSON'

    , isPushEvent
    , isPullRequestEvent
    , isStatusEvent
    , isIssueCommentEvent
    ) where

import           GHC.Generics

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import           Data.Text        (Text)

-- ----------------------------------------------

-- TODO: maybe use separate data types instead of sum type
--       which would make it easier to derive json instances and get rid of
--       parseXEvent functions etc.

{-# ANN type Event ("HLint: ignore Use camelCase" :: Text) #-}
data Event
  = PushEvent
    { epuRef         :: Text
    , epuCommits     :: [Commit]
    , epuHead_commit :: Commit
    , epuCompare     :: Text
    , epuRepository  :: Repository
    }
  | PullRequestEvent
    { eprAction       :: Text
    , eprNumber       :: Int
    , eprPull_request :: PullRequest
    , eprRepository   :: Repository
    }
  | StatusEvent
    { estSha         :: Text
    , estState       :: Text
    , estDescription :: Text
    , estStatus_url  :: Text
    , estRepository  :: Repository
    }
  | CommentEvent
    { ecoAction     :: Text
    , ecoIssue      :: Issue
    , ecoComment    :: Comment
    , ecoRepository :: Repository
    } deriving (Eq, Show, Generic)

instance FromJSON Event where
  parseJSON = genericParseJSON jsonParseOpts

data Commit = Commit
  { cmtId      :: Text
  , cmtMessage :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Commit where
  parseJSON = genericParseJSON jsonParseOpts


{-# ANN type Repository ("HLint: ignore Use camelCase" :: Text) #-}
data Repository = Repository
  { repName     :: Text
  , repHtml_url :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Repository where
  parseJSON = genericParseJSON jsonParseOpts


data Pusher = Pusher
  { email :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Pusher

{-# ANN type PullRequest ("HLint: ignore Use camelCase" :: Text) #-}
data PullRequest = PullRequest
  { purHtml_url :: Text
  , purState    :: Text
  , purTitle    :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON PullRequest where
  parseJSON = genericParseJSON jsonParseOpts

{-# ANN type Comment ("HLint: ignore Use camelCase" :: Text) #-}
data Comment = Comment
  { comHtml_url :: Text
  , comBody     :: Text
  , comUser     :: User
  } deriving (Eq, Show, Generic)

instance FromJSON Comment where
  parseJSON = genericParseJSON jsonParseOpts

{-# ANN type User ("HLint: ignore Use camelCase" :: Text) #-}
data User = User
  { usrLogin    :: Text
  , usrHtml_url :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON jsonParseOpts

{-# ANN type Issue ("HLint: ignore Use camelCase" :: Text) #-}
data Issue = Issue
  { issState    :: Text
  , issHtml_url :: Text
  , issUser     :: User
  -- XXX: add labels, assignee etc.
  } deriving (Eq, Show, Generic)

instance FromJSON Issue where
  parseJSON = genericParseJSON jsonParseOpts


-- TODO: smarter way to detect prefix (for camel and snake case)
jsonParseOpts = defaultOptions
  { fieldLabelModifier = labelModifier
  ,sumEncoding = ObjectWithSingleField
  }
  where
  labelModifier name = let (x:xs) = drop 3 name in switchCase x : xs -- XXX: don't forget the prefix or BOOM
  switchCase a = if isUpper a then toLower a else toUpper a

injectConstructor :: Text -> Value -> Value
injectConstructor h o = object [h .= o]

parseJSON' :: FromJSON a => Text -> Value -> Either String a
parseJSON' c = parseEither $ parseJSON . injectConstructor c

-- ----------------------------------------------

isPushEvent PushEvent{} = True
isPushEvent _           = False

isPullRequestEvent PullRequestEvent{} = True
isPullRequestEvent _                  = False

isStatusEvent StatusEvent{} = True
isStatusEvent _             = False

isIssueCommentEvent CommentEvent{} = True
isIssueCommentEvent _              = False

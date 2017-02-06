{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

-- | GitHub event types and instances.
module Github.Event.Types
    ( Event(..)
    , EventPayload(..)
    , Commit(..)
    , PushCommit(..)
    , Repository(..)
    , PullRequest(..)
    , Issue(..)
    , Comment(..)
    , User(..)
    , Review(..)

    , isPushEvent
    , isPullRequestEvent
    , isStatusEvent
    , isIssueCommentEvent
    , isPullRequestReviewEvent
    , isPullRequestReviewCommentEvent
    ) where

import           GHC.Generics

import           Data.Aeson
import           Data.Text        (Text)

import qualified JsonOptions      as Json

-- ----------------------------------------------

-- | An GitHub event including metadata.
data Event = Event
  { evtPayload :: EventPayload -- ^ The event payload
  , evtType    :: Text         -- ^ The event name, as set in the @X-GitHub-Event@ header
  , evtId      :: Text         -- ^ The delivery id, as set in the @X-GitHub-Delivery@ header
  } deriving (Show)


{-# ANN type EventPayload ("HLint: ignore Use camelCase" :: Text) #-}
-- | The GitHub event payload.
data EventPayload
  =
    -- | A <https://developer.github.com/v3/activity/events/types/#pushevent push> event.
    PushEvent
    { epuRef         :: Text
    , epuCommits     :: [PushCommit]
    , epuHead_commit :: PushCommit
    , epuCompare     :: Text
    , epuRepository  :: Repository
    }
    -- | A <https://developer.github.com/v3/activity/events/types/#pullrequestevent pull_request> event.
  | PullRequestEvent
    { eprAction       :: Text
    , eprNumber       :: Int
    , eprPull_request :: PullRequest
    , eprRepository   :: Repository
    }
    -- | A <https://developer.github.com/v3/activity/events/types/#statusevent status> event.
  | StatusEvent
    { estSha         :: Text
    , estState       :: Text
    , estDescription :: Maybe Text
    , estCommit      :: Commit
    , estTarget_url  :: Maybe Text
    , estRepository  :: Repository
    }
    -- | A <https://developer.github.com/v3/activity/events/types/#issuecommentevent issue_comment> event.
  | IssueCommentEvent
    { ecoAction     :: Text
    , ecoIssue      :: Issue
    , ecoComment    :: Comment
    , ecoRepository :: Repository
    }
    -- | A <https://developer.github.com/v3/activity/events/types/#pullrequestreviewevent pull_request_review> event.
  | PullRequestReviewEvent
    { ervAction       :: Text
    , ervReview       :: Review
    , ervPull_request :: PullRequest
    , ervRepository   :: Repository
    }
    -- | A <https://developer.github.com/v3/activity/events/types/#pullrequestreviewcommentevent pull_request_review_comment> event.
  | PullRequestReviewCommentEvent
    { ercAction       :: Text
    , ercComment      :: Comment
    , ercPull_request :: PullRequest
    , ercRepository   :: Repository
    } deriving (Eq, Show, Generic)

instance FromJSON EventPayload where
  parseJSON = genericParseJSON Json.parseOptions


data PushCommit = PushCommit
  { pcmId      :: Text
  , pcmMessage :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON PushCommit where
  parseJSON = genericParseJSON Json.parseOptions


{-# ANN type Commit ("HLint: ignore Use camelCase" :: Text) #-}
data Commit = Commit
  { cmtSha      :: Text
  , cmtHtml_url :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Commit where
  parseJSON = genericParseJSON Json.parseOptions


{-# ANN type Repository ("HLint: ignore Use camelCase" :: Text) #-}
data Repository = Repository
  { repName           :: Text
  , repDefault_branch :: Text
  , repHtml_url       :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Repository where
  parseJSON = genericParseJSON Json.parseOptions


data Pusher = Pusher
  { pshEmail :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Pusher

{-# ANN type PullRequest ("HLint: ignore Use camelCase" :: Text) #-}
data PullRequest = PullRequest
  { purNumber   :: Int
  , purHtml_url :: Text
  , purState    :: Text
  , purTitle    :: Text
  , purMerged   :: Maybe Bool
  } deriving (Eq, Show, Generic)

instance FromJSON PullRequest where
  parseJSON = genericParseJSON Json.parseOptions

{-# ANN type Comment ("HLint: ignore Use camelCase" :: Text) #-}
data Comment = Comment
  { comHtml_url :: Text
  , comBody     :: Text
  , comUser     :: User
  } deriving (Eq, Show, Generic)

instance FromJSON Comment where
  parseJSON = genericParseJSON Json.parseOptions

{-# ANN type User ("HLint: ignore Use camelCase" :: Text) #-}
data User = User
  { usrLogin    :: Text
  , usrHtml_url :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON Json.parseOptions

{-# ANN type Issue ("HLint: ignore Use camelCase" :: Text) #-}
data Issue = Issue
  { issState    :: Text
  , issHtml_url :: Text
  , issUser     :: User
  -- XXX: add labels, assignee etc.
  } deriving (Eq, Show, Generic)

instance FromJSON Issue where
  parseJSON = genericParseJSON Json.parseOptions

{-# ANN type Review ("HLint: ignore Use camelCase" :: Text) #-}
data Review = Review
  { revHtml_url :: Text
  , revBody     :: Text
  , revState    :: Text
  , revUser     :: User
  } deriving (Eq, Show, Generic)

instance FromJSON Review where
  parseJSON = genericParseJSON Json.parseOptions

-- ----------------------------------------------

-- | If the event is a push event.
isPushEvent :: EventPayload -> Bool
isPushEvent PushEvent{} = True
isPushEvent _           = False

-- | If the event is a pull request event.
isPullRequestEvent :: EventPayload -> Bool
isPullRequestEvent PullRequestEvent{} = True
isPullRequestEvent _                  = False

-- | If the event is a status event.
isStatusEvent :: EventPayload -> Bool
isStatusEvent StatusEvent{} = True
isStatusEvent _             = False

-- | If the event is an issue comment event.
isIssueCommentEvent :: EventPayload -> Bool
isIssueCommentEvent IssueCommentEvent{} = True
isIssueCommentEvent _                   = False

-- | If the event is a review event.
isPullRequestReviewEvent :: EventPayload -> Bool
isPullRequestReviewEvent PullRequestReviewEvent{} = True
isPullRequestReviewEvent _                        = False

-- | If the event is a review comment event.
isPullRequestReviewCommentEvent :: EventPayload -> Bool
isPullRequestReviewCommentEvent PullRequestReviewCommentEvent{} = True
isPullRequestReviewCommentEvent _                               = False

{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeOperators     #-}

-- | GitHub event types and instances.
module Github.Event.Types
    ( Event(..)
    , EventPayload(..)
    , StatusCommit(..)
    , Commit(..)
    , PushCommit(..)
    , Repository(..)
    , Committer(..)
    , PullRequest(..)
    , Issue(..)
    , Comment(..)
    , User(..)
    , Review(..)

    , isPingEvent
    , isPushEvent
    , isPullRequestEvent
    , isStatusEvent
    , isIssuesEvent
    , isIssueCommentEvent
    , isPullRequestReviewEvent
    , isPullRequestReviewCommentEvent

    , repository
    ) where

import           GHC.Generics

import           Data.Aeson
import           Data.Text    (Text)

import qualified JsonOptions  as Json

-- ----------------------------------------------
{-# ANN module ("HLint: ignore Use camelCase" :: Text) #-}
-- ----------------------------------------------

-- | An GitHub event including metadata.
data Event = Event
  { evtPayload :: EventPayload -- ^ The event payload
  , evtType    :: Text         -- ^ The event name, as set in the @X-GitHub-Event@ header
  , evtId      :: Text         -- ^ The delivery id, as set in the @X-GitHub-Delivery@ header
  } deriving (Show)


-- | The GitHub event payload.
data EventPayload
  =
    -- | A <https://developer.github.com/webhooks/#ping-event ping> event.
    PingEvent
    { epiZen        :: Text
    , epiHook_id    :: Int
    , epiRepository :: Repository
    }
    -- | A <https://developer.github.com/v3/activity/events/types/#pushevent push> event.
  | PushEvent
    { epuRef         :: Text
    , epuCommits     :: [PushCommit]
    , epuHead_commit :: Maybe PushCommit
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
    , estCommit      :: StatusCommit
    , estTarget_url  :: Maybe Text
    , estRepository  :: Repository
    }
    -- | A <https://developer.github.com/v3/activity/events/types/#issuesevent issues> event.
  | IssuesEvent
    { eisAction     :: Text
    , eisIssue      :: Issue
    , eisRepository :: Repository
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
  { pcmId        :: Text
  , pcmMessage   :: Text
  , pcmCommitter :: Committer
  } deriving (Eq, Show, Generic)

instance FromJSON PushCommit where
  parseJSON = genericParseJSON Json.parseOptions


data StatusCommit = StatusCommit
  { scmSha      :: Text
  , scmHtml_url :: Text
  , scmCommit   :: Commit
  } deriving (Eq, Show, Generic)

instance FromJSON StatusCommit where
  parseJSON = genericParseJSON Json.parseOptions


data Commit = Commit
  { cmtMessage :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Commit where
  parseJSON = genericParseJSON Json.parseOptions


data Repository = Repository
  { repName           :: Text
  , repFull_name      :: Text
  , repDefault_branch :: Text
  , repHtml_url       :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Repository where
  parseJSON = genericParseJSON Json.parseOptions


data Pusher = Pusher
  { pshEmail :: Text
  } deriving (Eq, Show, Generic)

instance ToJSON Pusher


data Committer = Committer
  { citEmail    :: Text
  , citUsername :: Text
  , citName     :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Committer where
  parseJSON = genericParseJSON Json.parseOptions


data PullRequest = PullRequest
  { purNumber    :: Int
  , purHtml_url  :: Text
  , purState     :: Text
  , purTitle     :: Text
  , purMerged    :: Maybe Bool
  , purMerged_by :: Maybe User
  , purUser      :: User
  } deriving (Eq, Show, Generic)

instance FromJSON PullRequest where
  parseJSON = genericParseJSON Json.parseOptions

data Comment = Comment
  { comHtml_url   :: Text
  , comBody       :: Text
  , comUser       :: User
  , comCreated_at :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON Comment where
  parseJSON = genericParseJSON Json.parseOptions

data User = User
  { usrLogin    :: Text
  , usrHtml_url :: Text
  } deriving (Eq, Show, Generic)

instance FromJSON User where
  parseJSON = genericParseJSON Json.parseOptions

data Issue = Issue
  { issNumber    :: Int
  , issState     :: Text
  , issTitle     :: Text
  , issBody      :: Text
  , issHtml_url  :: Text
  , issUser      :: User
  , issClosed_at :: Maybe Text
  -- XXX: add labels, assignee etc.
  } deriving (Eq, Show, Generic)

instance FromJSON Issue where
  parseJSON = genericParseJSON Json.parseOptions

data Review = Review
  { revHtml_url :: Text
  , revBody     :: Text
  , revState    :: Text
  , revUser     :: User
  } deriving (Eq, Show, Generic)

instance FromJSON Review where
  parseJSON = genericParseJSON Json.parseOptions

-- ----------------------------------------------

-- | If the event is a ping event.
isPingEvent :: EventPayload -> Bool
isPingEvent PingEvent{} = True
isPingEvent _           = False

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

-- | If the event is an issues event.
isIssuesEvent :: EventPayload -> Bool
isIssuesEvent IssuesEvent{} = True
isIssuesEvent _             = False

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

-- | The source repository of this event.
repository :: EventPayload -> Repository
repository PingEvent{..}                     = epiRepository
repository PushEvent{..}                     = epuRepository
repository PullRequestEvent{..}              = eprRepository
repository StatusEvent{..}                   = estRepository
repository IssuesEvent{..}                   = eisRepository
repository IssueCommentEvent{..}             = ecoRepository
repository PullRequestReviewEvent{..}        = ervRepository
repository PullRequestReviewCommentEvent{..} = ercRepository

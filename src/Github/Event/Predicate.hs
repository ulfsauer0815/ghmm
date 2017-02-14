{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

-- | GitHub event predicates.
--   Mostly for filtering events.
module Github.Event.Predicate
  ( isInterestingEvent

  , isClosingIssueComment
  ) where

import           Github.Api

-- ----------------------------------------------

-- | If an 'Event' is interesting enough to be considered for publication.
isInterestingEvent :: Event -> Bool
isInterestingEvent e
  =  isInterestingPush p
  || isInterestingPR p
  || isInterestingIssue p
  || isInterestingIssueComment p
  || isInterestingReview p
  || isInterestingReviewComment p
  || isInterestingStatus p
  || isPingEvent p
  where p = evtPayload e


isInterestingPush :: EventPayload -> Bool
-- ignore pushes that are the result of a GitHub PR merge using the web interface
isInterestingPush (PushEvent _ _ (Just (PushCommit _ _ (Committer email username _))) _ _)
  = not (email == "noreply@github.com" && username == "web-flow")
isInterestingPush PushEvent{} = True
isInterestingPush _ = False


isInterestingPR :: EventPayload -> Bool
isInterestingPR PullRequestEvent{..}
  =  eprAction == "opened"
  || eprAction == "synchronize"
  || eprAction == "closed"
  || eprAction == "reopened"
isInterestingPR _ = False


isInterestingIssue :: EventPayload -> Bool
isInterestingIssue IssuesEvent{..}
  =  eisAction == "opened"
  || eisAction == "closed"
  || eisAction == "reopened"
isInterestingIssue _ = False


isInterestingIssueComment :: EventPayload -> Bool
isInterestingIssueComment IssueCommentEvent{..}
  =  ecoAction == "created"
isInterestingIssueComment _ = False


isInterestingReview :: EventPayload -> Bool
isInterestingReview PullRequestReviewEvent{..}
  = True
isInterestingReview _ = False


isInterestingReviewComment :: EventPayload -> Bool
isInterestingReviewComment PullRequestReviewCommentEvent{..}
  =  ercAction == "created"
isInterestingReviewComment _ = False


isInterestingStatus :: EventPayload -> Bool
isInterestingStatus StatusEvent{..}
  =  estState /= "pending"
isInterestingStatus _ = False


-- | If an event is the accompanying comment to the closing of an issue.
isClosingIssueComment :: EventPayload -> Bool
isClosingIssueComment IssueCommentEvent{..}
  =  issClosed_at ecoIssue == Just (comCreated_at ecoComment)
isClosingIssueComment _ = False

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Github.Event.Filter
  ( isInterestingEvent
  ) where

import           Github.Api

-- ----------------------------------------------

isInterestingEvent :: Event -> Bool
isInterestingEvent e
  =  isInterestingPR p
  || isInterestingComment p
  || isInterestingReviewComment p
  || isInterestingStatus p
  where p = evtPayload e


isInterestingPR :: EventPayload -> Bool
isInterestingPR PullRequestEvent{..}
  =  eprAction == "opened"
  || eprAction == "synchronize"
  || eprAction == "closed"
  || eprAction == "reopened"
isInterestingPR _ = False


isInterestingComment :: EventPayload -> Bool
isInterestingComment IssueCommentEvent{..}
  =  ecoAction == "created"
isInterestingComment _ = False


isInterestingReviewComment :: EventPayload -> Bool
isInterestingReviewComment PullRequestReviewCommentEvent{..}
  =  ercAction == "created"
isInterestingReviewComment _ = False


isInterestingStatus :: EventPayload -> Bool
isInterestingStatus StatusEvent{..}
  =  estState /= "pending"
isInterestingStatus _ = False

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Github.Event.Filter
  ( isInterestingEvent
  ) where

import           Github.Api

-- ----------------------------------------------

isInterestingEvent :: Event -> Bool
isInterestingEvent e
  =  isInterestingPR e
  && isInterestingComment e
  && isInterestingReviewComment e
  && isInterestingStatus e


isInterestingPR :: Event -> Bool
isInterestingPR PullRequestEvent{..}
  =  eprAction == "opened"
  || eprAction == "synchronize"
  || eprAction == "closed"
  || eprAction == "reopened"
isInterestingPR _ = False


isInterestingComment :: Event -> Bool
isInterestingComment IssueCommentEvent{..}
  =  ecoAction == "created"
isInterestingComment _ = False


isInterestingReviewComment :: Event -> Bool
isInterestingReviewComment PullRequestReviewCommentEvent{..}
  =  ercAction == "created"
isInterestingReviewComment _ = False


isInterestingStatus :: Event -> Bool
isInterestingStatus StatusEvent{..}
  =  estState /= "pending"
isInterestingStatus _ = False

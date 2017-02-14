{-# LANGUAGE OverloadedStrings #-}

module Github.Event.JsonSpec
    ( main
    , spec
    ) where

import           Test.Hspec
import           Test.QuickCheck.Instances ()

import           Data.Aeson                hiding (json)
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as BL
import           Data.Either
import           Data.Maybe
import           Data.Text                 (Text)

import           Github.Event.Json
import           Github.Event.Predicate
import           Github.Event.Types

import           Util

-- ----------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Github.Event.Json.decodeEvent" $ do
    it "decodes \"push\" event" $ do
      event <- loadAndCheckEvent "push.json" "push"
      evtPayload event `shouldSatisfy` isPushEvent
      event            `shouldSatisfy` isInterestingEvent

    it "decodes \"push\" event by GitHub and is boring" $ do
      event <- loadAndCheckEvent "push_pr_merge.json" "push"
      evtPayload event `shouldSatisfy` isPushEvent
      event            `shouldNotSatisfy` isInterestingEvent

    it "decodes \"push\" event with null values" $ do
      event <- loadAndCheckEvent "push_null.json" "push"
      evtPayload event `shouldSatisfy` isPushEvent
      event            `shouldSatisfy` isInterestingEvent

    it "decodes \"pull_request\" event" $ do
      event <- loadAndCheckEvent "pullrequest.json" "pull_request"
      evtPayload event `shouldSatisfy` isPullRequestEvent
      event            `shouldSatisfy` isInterestingEvent

    it "decodes \"status\" event" $ do
      event <- loadAndCheckEvent "status.json" "status"
      evtPayload event `shouldSatisfy` isStatusEvent
      event            `shouldSatisfy` isInterestingEvent

    it "decodes \"status\" event with description" $ do
      event <- loadAndCheckEvent "status_with_description.json" "status"
      evtPayload event `shouldSatisfy` isStatusEvent
      event            `shouldSatisfy` isInterestingEvent

    it "decodes \"issues\" event" $ do
      event <- loadAndCheckEvent "issues.json" "issues"
      evtPayload event `shouldSatisfy` isIssuesEvent
      event            `shouldSatisfy` isInterestingEvent

    it "decodes \"issue_comment\" event" $ do
      event <- loadAndCheckEvent "issuecomment.json" "issue_comment"
      evtPayload event `shouldSatisfy`    isIssueCommentEvent
      event            `shouldSatisfy`    isInterestingEvent
      evtPayload event `shouldNotSatisfy` isClosingIssueComment

    it "decodes \"issue_comment\" event which closes an issue" $ do
      event <- loadAndCheckEvent "issuecomment_closing.json" "issue_comment"
      evtPayload event `shouldSatisfy` isIssueCommentEvent
      event            `shouldSatisfy` isInterestingEvent
      evtPayload event `shouldSatisfy` isClosingIssueComment

    it "decodes \"pull_request_review\" event" $ do
      event <- loadAndCheckEvent "pullrequestreview.json" "pull_request_review"
      evtPayload event `shouldSatisfy` isPullRequestReviewEvent
      event            `shouldSatisfy` isInterestingEvent

    it "decodes \"pull_request_review_comment\" event" $ do
      event <- loadAndCheckEvent "pullrequestreviewcomment.json" "pull_request_review_comment"
      evtPayload event `shouldSatisfy` isPullRequestReviewCommentEvent
      event            `shouldSatisfy` isInterestingEvent

-- ----------------------------------------------

loadAndCheckEvent :: String -> Text -> IO Event
loadAndCheckEvent file header = do
  json <- loadFile file
  let valueMb = decode json :: Maybe Value
  valueMb `shouldSatisfy` isJust
  let value = fromJust valueMb
  let eventEt = decodeEvent header value :: Either String EventPayload
  eventEt `shouldSatisfy` isRight
  return $ Event (fromRight eventEt) header "delivery-id"

loadFile :: String -> IO ByteString
loadFile relPath = BL.readFile $ "test/data/event/" ++ relPath

{-# LANGUAGE OverloadedStrings #-}

module Github.Event.JsonSpec
    ( main
    , spec
    ) where

import           System.Environment

import           Test.Hspec
import           Test.QuickCheck.Instances ()

import           Data.Aeson                hiding (json)
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as BL
import           Data.Either
import           Data.Maybe
import           Data.Monoid
import           Data.Text                 (Text)
import qualified Data.Text.IO              as T

import           Github.Event.Json
import           Github.Event.Message
import           Github.Event.Types

import           Util

-- ----------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  logFile <- runIO $ lookupEnv "TEST_LOG"
  let logMessage e = case logFile of
        Nothing -> return ()
        Just file ->
          T.appendFile file $ renderMessageText e <> "\n\n"

  describe "Github.Event.Json.decodeEvent" $ do
    it "decodes \"push\" event" $ do
      event <- loadAndCheckEvent "push.json" "push"
      event `shouldSatisfy` isPushEvent
      logMessage event

    it "decodes \"pull_request\" event" $ do
      event <- loadAndCheckEvent "pullrequest.json" "pull_request"
      event `shouldSatisfy` isPullRequestEvent
      logMessage event

    it "decodes \"status\" event" $ do
      event <- loadAndCheckEvent "status.json" "status"
      event `shouldSatisfy` isStatusEvent

    it "decodes \"status\" event with description" $ do
      event <- loadAndCheckEvent "status_with_description.json" "status"
      event `shouldSatisfy` isStatusEvent
      logMessage event

    it "decodes \"issue_comment\" event" $ do
      event <- loadAndCheckEvent "issuecomment.json" "issue_comment"
      event `shouldSatisfy` isIssueCommentEvent
      logMessage event

    it "decodes \"pull_request_review\" event" $ do
      event <- loadAndCheckEvent "pullrequestreview.json" "pull_request_review"
      event `shouldSatisfy` isPullRequestReviewEvent
      logMessage event

    it "decodes \"pull_request_review_comment\" event" $ do
      event <- loadAndCheckEvent "pullrequestreviewcomment.json" "pull_request_review_comment"
      event `shouldSatisfy` isPullRequestReviewCommentEvent
      logMessage event

-- ----------------------------------------------

loadAndCheckEvent :: String -> Text -> IO EventPayload
loadAndCheckEvent file header = do
  json <- loadFile file
  let valueMb = decode json :: Maybe Value
  valueMb `shouldSatisfy` isJust
  let value = fromJust valueMb
  let eventEt = decodeEvent header value :: Either String EventPayload
  eventEt `shouldSatisfy` isRight
  return $ fromRight eventEt

loadFile :: String -> IO ByteString
loadFile relPath = BL.readFile $ "test/data/event/" ++ relPath

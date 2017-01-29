{-# LANGUAGE OverloadedStrings #-}

module Github.Event.JsonSpec
    ( main
    , spec
    ) where

import           Test.Hspec
import           Test.QuickCheck.Instances ()

import           Data.Aeson
import           Data.ByteString.Lazy      (ByteString)
import qualified Data.ByteString.Lazy      as BL
import           Data.Maybe
import           Data.Text                 (Text)

import           Github.Event.Json
import           Github.Event.Types

-- ----------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Github.Event.Json.decodeEvent" $ do
    it "decodes \"push\" event" $ do
      event <- loadAndCheckEvent "pushevent.json" "push"
      event `shouldSatisfy` isPushEvent

    it "decodes \"pull_request\" event" $ do
      event <- loadAndCheckEvent "pullrequestevent.json" "pull_request"
      event `shouldSatisfy` isPullRequestEvent

-- ----------------------------------------------

loadAndCheckEvent :: String -> Text -> IO Event
loadAndCheckEvent file header = do
  json <- loadFile file
  let eventMb = parseEvent header json :: Maybe Event
  eventMb `shouldSatisfy` isJust
  return $ fromJust eventMb

parseEvent :: Text -> ByteString -> Maybe Event
parseEvent header rawJson = do
  event <- decode rawJson :: Maybe Value
  decodeEvent header event

loadFile :: String -> IO ByteString
loadFile relPath = BL.readFile $ "test/data/" ++ relPath

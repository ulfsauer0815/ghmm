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
import           Data.Either
import           Data.Maybe
import           Data.Text                 (Text)

import           Github.Event.Json
import           Github.Event.Types

import           Util

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

    it "decodes \"status\" event" $ do
      event <- loadAndCheckEvent "statusevent.json" "status"
      event `shouldSatisfy` isStatusEvent

-- ----------------------------------------------

loadAndCheckEvent :: String -> Text -> IO Event
loadAndCheckEvent file header = do
  json <- loadFile file
  let valueMb = decode json :: Maybe Value
  valueMb `shouldSatisfy` isJust
  let value = fromJust valueMb
  let eventEt = decodeEvent header value :: Either String Event
  eventEt `shouldSatisfy` isRight
  return $ fromRight eventEt

loadFile :: String -> IO ByteString
loadFile relPath = BL.readFile $ "test/data/" ++ relPath

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
  describe "Github.Event.Json.decodeEvent" $
    it "decodes \"push\" event" $ do
      json <- loadFile "pushevent.json"
      let event = parseEvent "push" json :: Maybe Event
      event `shouldSatisfy` isJust

-- ----------------------------------------------

parseEvent :: Text -> ByteString -> Maybe Event
parseEvent header rawJson = do
  event <- decode rawJson :: Maybe Value
  decodeEvent header event

loadFile :: String -> IO ByteString
loadFile relPath = BL.readFile $ "test/data/" ++ relPath

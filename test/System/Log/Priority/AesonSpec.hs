{-# LANGUAGE OverloadedStrings #-}

module System.Log.Priority.AesonSpec
    ( main
    , spec
    ) where

import           Test.Hspec
import           Test.QuickCheck.Instances ()

import           Data.Aeson

import           Config                    (Priority (..))

-- ----------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "System.Log.Priority (FromJSON)" $ do
    it "deserializes DEBUG" $ do
      let priority = decode "\"DEBUG\""
      priority `shouldBe` Just DEBUG
    it "deserializes INFO" $ do
      let priority = decode "\"INFO\""
      priority `shouldBe` Just INFO
    it "deserializes NOTICE" $ do
      let priority = decode "\"NOTICE\""
      priority `shouldBe` Just NOTICE
    it "deserializes WARNING" $ do
      let priority = decode "\"WARNING\""
      priority `shouldBe` Just WARNING
    it "deserializes ERROR" $ do
      let priority = decode "\"ERROR\""
      priority `shouldBe` Just ERROR
    it "deserializes CRITICAL" $ do
      let priority = decode "\"CRITICAL\""
      priority `shouldBe` Just CRITICAL
    it "deserializes ALERT" $ do
      let priority = decode "\"ALERT\""
      priority `shouldBe` Just ALERT
    it "deserializes EMERGENCY" $ do
      let priority = decode "\"EMERGENCY\""
      priority `shouldBe` Just EMERGENCY
    it "deserializes EMERGENCY ignoring case" $ do
      let priority = decode "\"eMeRgEnCy\""
      priority `shouldBe` Just EMERGENCY

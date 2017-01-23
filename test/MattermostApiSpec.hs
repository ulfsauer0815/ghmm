{-# LANGUAGE OverloadedStrings   #-}

module MattermostApiSpec (main, spec) where

import           Test.Hspec
import           Test.QuickCheck
import           Test.QuickCheck.Instances ()

import qualified Data.Text                 as T

import           MattermostApi

-- ----------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "MattermostApi.shorten" $ do
    it "shortens text without marker" $
      shorten 5    ""          "1234567890" `shouldBe` "12345"
    it "shortens text with marker" $
      shorten 5 "..."          "1234567890" `shouldBe` "12..."
    it "does not shorten text if marker is just as long" $
      shorten 5 "............" "1234567890" `shouldBe` "1234567890"
    it "shortens text by replacing it entirely by the shorter marker" $
      shorten 3 "..."          "1234567890" `shouldBe` "..."
    it "shortens text by replacing everything but one character" $
      shorten 9 "........"     "1234567890" `shouldBe` "1........"

    it "shortens text (prop)" $
      property $ \(NonNegative maxLen) shortenedMarker str -> T.length (shorten maxLen shortenedMarker str) == min (T.length str) (max maxLen (T.length shortenedMarker))

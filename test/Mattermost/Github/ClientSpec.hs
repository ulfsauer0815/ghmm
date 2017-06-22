{-# LANGUAGE OverloadedStrings #-}

module Mattermost.Github.ClientSpec
    ( main
    , spec
    ) where

import           Test.Hspec
import           Test.QuickCheck.Instances ()

import           Data.Map                  (Map)
import qualified Data.Map                  as M
import           Data.Text                 (Text)

import           Config
import           Mattermost.Github.Client

-- ----------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec = do
    let mapping = M.fromList
            [ ("org"         , RepositoryConfig {rcgChannel = Just "#org"           , rcgBot = Nothing})
            , ("org/repo"    , RepositoryConfig {rcgChannel = Just "#org/repo"      , rcgBot = Nothing})
            , ("org2/repo2"  , RepositoryConfig {rcgChannel = Just "#org2/repo2"    , rcgBot = Nothing})
            , ("org3/repo2"  , RepositoryConfig {rcgChannel = Just "#org2/repo2"    , rcgBot = Nothing})
            ]
        mappingWithDef = M.insert "_default" RepositoryConfig {rcgChannel = Just "#default", rcgBot = Nothing} mapping

    describe "Mattermost.Github.Client.matchRepository without default" $ do
      it "matches direct mapping" $
        "org/repo"          `matchChannel` mapping `shouldBe` Just "#org/repo"
      it "matches second direct mapping" $
        "org2/repo2"        `matchChannel` mapping `shouldBe` Just "#org2/repo2"
      it "matches org mapping" $
        "org"               `matchChannel` mapping `shouldBe` Just "#org"
      it "matches org mapping as fallback" $
        "org/unknown"       `matchChannel` mapping `shouldBe` Just "#org"
      it "does not match repo on prefix" $
        "org2/rep"          `matchChannel` mapping `shouldBe` Nothing
      it "does not match anything" $
        "unknown/unknown"   `matchChannel` mapping `shouldBe` Nothing

    describe "Mattermost.Github.Client.matchRepository with default" $ do
      it "matches direct mapping ignoring default" $
        "org/repo"          `matchChannel` mappingWithDef `shouldBe` Just "#org/repo"
      it "matches second direct mapping ignoring default" $
        "org2/repo2"        `matchChannel` mappingWithDef `shouldBe` Just "#org2/repo2"
      it "matches org mapping ignoring default" $
        "org"               `matchChannel` mappingWithDef `shouldBe` Just "#org"
      it "matches org mapping as fallback ignoring default" $
        "org/unknown"       `matchChannel` mappingWithDef `shouldBe` Just "#org"
      it "falls back to default when it does not match repo on prefix" $
        "org2/rep"          `matchChannel` mappingWithDef `shouldBe` Just "#default"
      it "falls back to default when does not match anything" $
        "unknown/unknown"   `matchChannel` mappingWithDef `shouldBe` Just "#default"


matchChannel :: Text -> Map Text RepositoryConfig -> Maybe Text
matchChannel repo mapping = matchRepository repo mapping >>= rcgChannel

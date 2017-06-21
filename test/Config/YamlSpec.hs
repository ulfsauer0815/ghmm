{-# LANGUAGE OverloadedStrings #-}

module Config.YamlSpec
    ( main
    , spec
    ) where

import           Test.Hspec
import           Test.QuickCheck.Instances ()

import qualified Data.Map                  as M

import           Servant.Client

import           Config

-- ----------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Config.loadConfig: YAML" $ do
    it "loads full configuration" $ do
      let expected = Config
            { cfgPort = 12345
            , cfgPriority = DEBUG
            , cfgGithubSecret = Just "abc"
            , cfgMattermostUrl = BaseUrl Https "mattermost.invalid" 443 ""
            , cfgMattermostApiKey = "xyz"
            , cfgRepositories = M.fromList
              [ ("org"         , RepositoryConfig {rcgChannel = Just "what"})
              , ("org/repo"    , RepositoryConfig {rcgChannel = Just "is"})
              , ("org2/repo2"  , RepositoryConfig {rcgChannel = Just "this"})
              ]
            }
      loadConfig' "config_full.yml"
        `shouldReturn` Right expected

    it "loads sparse configuration" $ do
      let expected = Config
            { cfgPort = 8000
            , cfgPriority = ERROR
            , cfgGithubSecret = Nothing
            , cfgMattermostUrl = BaseUrl Https "mattermost.invalid" 443 ""
            , cfgMattermostApiKey = "xyz"
            , cfgRepositories = M.empty
            }
      loadConfig' "config_sparse.yml"
        `shouldReturn` Right expected

-- ----------------------------------------------

loadConfig' :: FilePath -> IO (Either String Config)
loadConfig' relPath = loadConfig $ Just $ "test/data/config/" ++ relPath


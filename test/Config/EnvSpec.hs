{-# LANGUAGE OverloadedStrings #-}

module Config.EnvSpec
    ( main
    , spec
    ) where

import           Test.Hspec
import           Test.QuickCheck.Instances ()

import           System.Environment

import qualified Data.Map                  as M

import           Servant.Client

import           Config

-- ----------------------------------------------

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  resetEnvRule $
    describe "Config.loadConfig: Env" $
      it "loads full configuration" $ do
        mapM_ (uncurry setEnv)
          [ ("PORT",                "12345")
          , ("LOG_LEVEL",           "DEBUG")
          , ("MATTERMOST_URL",      "https://mattermost.invalid")
          , ("MATTERMOST_API_KEY",  "xyz")
          , ("MATTERMOST_CHANNEL",  "test-channel")
          , ("GITHUB_SECRET",       "abc")
          ]
        let expected = Config
              { cfgPort = 12345
              , cfgPriority = DEBUG
              , cfgGithubSecret = Just "abc"
              , cfgMattermostUrl = BaseUrl Https "mattermost.invalid" 443 ""
              , cfgMattermostApiKey = "xyz"
              , cfgRepositories = M.fromList
                [ ("_default"    , RepositoryConfig {rcgChannel = Just "test-channel", rcgBot = Nothing})]
              }
        loadConfig' `shouldReturn` Right expected

-- ----------------------------------------------

loadConfig' :: IO (Either String Config)
loadConfig' = loadConfig Nothing

resetEnvRule :: SpecWith a -> SpecWith a
resetEnvRule = around_ $ \test -> resetEnv >> test >> resetEnv
  where
  resetEnv = mapM_ unsetEnv usedEnvVars
  usedEnvVars = [ "PORT", "LOG_LEVEL", "GITHUB_SECRET", "MATTERMOST_URL", "MATTERMOST_API_KEY", "MATTERMOST_CHANNEL"]

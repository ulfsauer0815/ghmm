{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}

module Config.Env where

-- ----------------------------------------------

import           System.Log            (Priority)
import qualified System.Log.Logger     as Log

import           Data.ByteString       (ByteString)
import           Data.ByteString.Aeson ()
import qualified Data.Map              as M
import           Data.Text             (Text)

import           Servant.Client
import           Servant.Client.Aeson  ()

import           Config.Env.Util
import           Config.Types
import           Util

-- ----------------------------------------------

-- | The app configuration via environment variables.
data EnvConfig = EnvConfig
  { efgPort              :: Int               -- ^ Port to listen on
  , efgPriority          :: Priority          -- ^ Debug level
  , efgGithubSecret      :: Maybe ByteString  -- ^ Shared secret for the incoming GitHub webhook calls
  , efgMattermostUrl     :: BaseUrl           -- ^ Mattermost base URL
  , efgMattermostApiKey  :: Text              -- ^ Mattermost API key - the last part of the incoming webhook URL
  , efgMattermostChannel :: Maybe Text        -- ^ Mattermost channel send messages to
  }

-- ----------------------------------------------

-- | Convert the environment configuration to the (canonical) configuration.
fromEnvConfig :: EnvConfig -> Config
fromEnvConfig EnvConfig{..} = Config
  { cfgPort             = efgPort
  , cfgPriority         = efgPriority
  , cfgGithubSecret     = efgGithubSecret
  , cfgMattermostUrl    = efgMattermostUrl
  , cfgMattermostApiKey = efgMattermostApiKey
  , cfgRepositories     = M.fromList [("_default", RepositoryConfig efgMattermostChannel Nothing)]
  }

-- ----------------------------------------------

-- | Reads the configuration from environment variables.
--   If a property is missing, it will fail.
--   If a property is malformed it will either silently fall back to the default
--   or fail as well.
readEnvConfig' :: ConfigReader Config
readEnvConfig' = fmap fromEnvConfig $
  EnvConfig
    <$> envRead       "PORT"                 `withDef` 8000
    <*> envRead       "LOG_LEVEL"            `withDef` Log.ERROR
    <*> envBS   `opt` "GITHUB_SECRET"        `withDef` Nothing
    <*> envUrl        "MATTERMOST_URL"
    <*> env           "MATTERMOST_API_KEY"
    <*> env     `opt` "MATTERMOST_CHANNEL"   `withDef` Nothing
    where
    envUrl :: Text -> ConfigReader BaseUrl
    envUrl = env' parseBaseUrl

-- ----------------------------------------------

readEnvConfig :: IO (Either String Config)
readEnvConfig = maybeToEither "Incomplete/invalid configuration" <$> runConfigReader readEnvConfig'

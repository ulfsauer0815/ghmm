{-# LANGUAGE DeriveGeneric #-}

module Config.Types where

-- ----------------------------------------------

import           GHC.Generics

import           System.Log            (Priority)
import           System.Log.Aeson      ()

import           Data.Aeson
import           Data.ByteString       (ByteString)
import           Data.ByteString.Aeson ()
import           Data.Map              (Map)
import           Data.Text             (Text)

import           Servant.Client
import           Servant.Client.Aeson  ()

import qualified JsonOptions           as Json

-- ---------------------------------------------

-- | The app configuration.
data Config = Config
  { cfgPort             :: Int                         -- ^ Port to listen on
  , cfgPriority         :: Priority                    -- ^ Debug level
  , cfgGithubSecret     :: Maybe ByteString            -- ^ Shared secret for the incoming GitHub webhook calls
  , cfgMattermostUrl    :: BaseUrl                     -- ^ Mattermost base URL
  , cfgMattermostApiKey :: Text                        -- ^ Mattermost API key - the last part of the incoming webhook URL
  , cfgRepositories     :: Map Text RepositoryConfig   -- ^ Mapping of Github org/repo names to Mattermost channels
  } deriving (Eq, Show)

-- ----------------------------------------------

data RepositoryConfig = RepositoryConfig
  { rcgChannel :: Maybe Text
  } deriving (Eq, Show, Generic)

instance FromJSON RepositoryConfig where
  parseJSON = genericParseJSON Json.parseOptions

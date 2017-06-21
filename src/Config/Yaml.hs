{-# LANGUAGE DeriveGeneric   #-}
{-# LANGUAGE RecordWildCards #-}

module Config.Yaml where

-- ----------------------------------------------

import           GHC.Generics

import           System.Log            (Priority)
import           System.Log.Aeson      ()
import qualified System.Log.Logger     as L

import           Data.Aeson
import           Data.ByteString       (ByteString)
import           Data.ByteString.Aeson ()
import           Data.Map              (Map)
import qualified Data.Map              as M
import           Data.Maybe
import           Data.Text             (Text)
import           Data.Yaml

import           Servant.Client
import           Servant.Client.Aeson  ()

import qualified JsonOptions           as Json

import           Config.Types

-- ----------------------------------------------

-- | The app configuration (via YAML file).
data YamlConfig = YamlConfig
  { yfgPort         :: Maybe Int                           -- ^ Port to listen on
  , yfgLogging      :: Maybe LoggingConfig                 -- ^ Logging confi
  , yfgGithub       :: Maybe GithubConfig                  -- ^ Github config
  , yfgMattermost   :: MattermostConfig                    -- ^ Mattermost base URL
  , yfgRepositories :: Maybe (Map Text RepositoryConfig)   -- ^ Mapping of Github org/repo names to Mattermost channels
  } deriving (Eq, Show, Generic)

instance FromJSON YamlConfig where
  parseJSON = genericParseJSON Json.parseOptions

data LoggingConfig = LoggingConfig
  { lgcPriority :: Maybe Priority -- ^ Debug level
  } deriving (Eq, Show, Generic)

instance FromJSON LoggingConfig where
  parseJSON = genericParseJSON Json.parseOptions

data GithubConfig = GithubConfig
  { ghcSecret :: Maybe ByteString -- ^ Shared secret for the incoming GitHub webhook calls
  } deriving (Eq, Show, Generic)

instance FromJSON GithubConfig where
  parseJSON = genericParseJSON Json.parseOptions

data MattermostConfig = MattermostConfig
  { mmcUrl    :: BaseUrl -- ^ Mattermost base URL
  , mmcApiKey :: Text    -- ^ Mattermost API key - the last part of the incoming webhook URL
  } deriving (Eq, Show, Generic)

instance FromJSON MattermostConfig where
  parseJSON = genericParseJSON Json.parseOptions

 -- ----------------------------------------------

-- TODO: sync defaults with env cfg
-- | Convert the environment configuration to the (canonical) configuration.
fromYamlConfig :: YamlConfig -> Config
fromYamlConfig YamlConfig{..} = Config
  { cfgPort             = fromMaybe 8000 yfgPort
  , cfgPriority         = fromMaybe L.ERROR $ yfgLogging >>= lgcPriority
  , cfgGithubSecret     = yfgGithub >>= ghcSecret
  , cfgMattermostUrl    = mmcUrl yfgMattermost
  , cfgMattermostApiKey = mmcApiKey yfgMattermost
  , cfgRepositories     = fromMaybe M.empty yfgRepositories
  }

 -- ----------------------------------------------

-- | Reads the YAML configuration.
readYamlConfig' :: FilePath -> IO (Either ParseException Config)
readYamlConfig' f =
  fmap fromYamlConfig <$> (decodeFileEither f :: IO (Either ParseException YamlConfig))

 -- ----------------------------------------------

readYamlConfig :: FilePath -> IO (Either String Config)
readYamlConfig file = fmapLeft show <$> readYamlConfig' file
  where
  fmapLeft :: (l1 -> l2) -> Either l1 r -> Either l2 r
  fmapLeft f (Left v)  = Left $ f v
  fmapLeft _ (Right v) = Right v

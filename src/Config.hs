module Config
  ( Config(..)
  , RepositoryConfig(..)

  , Priority(..)

  , BaseUrl(..)
  , parseBaseUrl
  , showBaseUrl

  , loadConfig
  ) where

import           Config.Env
import           Config.Types
import           Config.Yaml

import           System.Log       (Priority (..))
import           System.Log.Aeson ()

import           Servant.Client

-- ----------------------------------------------

loadConfig :: Maybe FilePath -> IO (Either String Config)
loadConfig mbFile
  = case mbFile of
      Just file -> readYamlConfig file
      Nothing   -> readEnvConfig

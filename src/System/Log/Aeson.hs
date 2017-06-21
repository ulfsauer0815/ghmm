{-# OPTIONS_GHC -fno-warn-orphans #-}

module System.Log.Aeson where

import           System.Log       (Priority (..))

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Map         (Map)
import qualified Data.Map         as M
import           Data.Text        (Text)
import qualified Data.Text        as T

-- ----------------------------------------------

instance FromJSON Priority where
  parseJSON v@(String s) = case M.lookup (T.toLower s) prioMap of
    Just v' -> return v'
    Nothing -> typeMismatch "Priority" v
  parseJSON v = typeMismatch "Priority" v


prioMap :: Map Text Priority
prioMap = M.fromList $ map (\p -> (T.toLower . T.pack . show $ p, p))
  [DEBUG, INFO, NOTICE, WARNING, ERROR, CRITICAL, ALERT, EMERGENCY]

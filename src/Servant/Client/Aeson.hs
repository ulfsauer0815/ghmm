{-# OPTIONS_GHC -fno-warn-orphans #-}

module Servant.Client.Aeson where

import           Data.Aeson
import           Data.Aeson.Types
import qualified Data.Text        as T

import           Servant.Client   (BaseUrl, parseBaseUrl)

-- ----------------------------------------------

instance FromJSON BaseUrl where
  parseJSON (String s) = case parseBaseUrl $ T.unpack s of
    Right v -> return v
    Left  e -> fail . show $ e
  parseJSON v = typeMismatch "BaseUrl" v


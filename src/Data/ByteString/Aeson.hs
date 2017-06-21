{-# OPTIONS_GHC -fno-warn-orphans #-}

module Data.ByteString.Aeson where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString
import qualified Data.Text.Encoding as T

-- ----------------------------------------------

instance FromJSON ByteString where
  parseJSON (String s) = pure . T.encodeUtf8 $ s
  parseJSON v          = typeMismatch "ByteString" v

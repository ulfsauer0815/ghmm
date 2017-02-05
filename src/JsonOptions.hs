module JsonOptions
  ( parseOptions
  , encodingOptions
  ) where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char

-- ----------------------------------------------

parseOptions :: Options
parseOptions = defaultOptions
  { fieldLabelModifier = labelModifier
  , sumEncoding        = ObjectWithSingleField
  }
  where
  labelModifier name = let (x:xs) = drop 3 name in switchCase x : xs -- XXX: don't forget the prefix or BOOM
  switchCase a = if isUpper a then toLower a else toUpper a


encodingOptions :: Options
encodingOptions = defaultOptions
  { fieldLabelModifier = labelModifier
  }
  where
  labelModifier name = let (x:xs) = drop 3 name in switchCase x : xs -- XXX: don't forget the prefix or BOOM
  switchCase a = if isUpper a then toLower a else toUpper a

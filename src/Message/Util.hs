{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions.related to messages.
module Message.Util
    ( shorten
    ) where

import           Data.Text   (Text)
import qualified Data.Text   as T

-- ----------------------------------------------

-- | Shorten a message to a certain length and a replacement marker at the end.
shorten :: Int -> Text -> Text -> Text
shorten maxLen marker str =
  if len > maxLen && len > max maxLen' markerLen
  then shortened
  else str
  where
  len = T.length str
  markerLen = T.length marker
  maxLen' = maxLen - markerLen

  shortened = T.take maxLen' str `T.append` marker

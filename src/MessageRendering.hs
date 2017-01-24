{-# LANGUAGE OverloadedStrings #-}

module MessageRendering
    ( shorten
    , link
    , quote
    , codeblock
    , bold
    , italic
    ) where

import           Data.Monoid
import           Data.String (IsString)
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

-- ----------------------------------------------
-- Markdown
-- ----------------------------------------------

-- | Text as a codeblock
codeblock :: (Monoid m, IsString m) => m -> m
codeblock text = "```" <> text <> "```"

-- | Linkified text
link :: (Monoid m, IsString m) => m -> m -> m
link text url = "[" <> text <> "](" <> url <> ")"

-- | Text put in quotes
quote :: (Monoid m, IsString m) => m -> m
quote text = "\"" <> text <> "\""

-- | Text in italics
italic :: (Monoid m, IsString m) => m -> m
italic text = "*" <> text <> "*"

-- | Text in bold
bold :: (Monoid c, IsString c) => t -> c -> c
bold text = italic . italic

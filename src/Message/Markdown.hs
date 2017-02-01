{-# LANGUAGE OverloadedStrings #-}

module Message.Markdown
    ( quote
    , italic
    , bold
    , codeblock
    , link
    ) where

import           Data.Monoid
import           Data.String (IsString)

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
bold :: (Monoid c, IsString c) => c -> c
bold = italic . italic

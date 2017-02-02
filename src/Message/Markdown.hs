{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}

module Message.Markdown
    ( quote
    , ml
    , mr
    , ma
    , tl

    , italic
    , bold
    , codeblock
    , link
    ) where

import           Data.Monoid
import           Data.String (IsString)

-- ----------------------------------------------

type Stringish s = (Monoid s, IsString s, Eq s)

-- | Text as a codeblock
codeblock :: Stringish m => m -> m
codeblock = nonNull $ \text -> "```" <> text <> "```"

-- | Linkified text
link :: Stringish m => m -> m -> m
link text url = nonNull (\text' -> "[" <> text' <> "](" <> url <> ")") text

-- | Text put in quotes
quote :: Stringish m => m -> m
quote = nonNull $ \text -> "\"" <> text <> "\""

-- | Text in italics
italic :: Stringish m => m -> m
italic = nonNull $ \text -> "*" <> text <> "*"

-- | Text in bold
bold :: Stringish m => m -> m
bold = italic . italic

-- | Margin left
ml :: Stringish m => m -> m
ml = tl " "

-- | Margin right
mr :: Stringish m => m -> m
mr = tr " "

-- | Margin right
ma :: Stringish m => m -> m
ma = mr . ml

tl :: Stringish m => m -> m -> m
tl prefix = nonNull $ \text -> prefix <> text

tr :: Stringish m => m -> m -> m
tr suffix = nonNull $ \text -> text <> suffix

-- | Applies f if m is non-empty
nonNull :: Stringish m  => (m -> m) -> m -> m
nonNull f m = if m == mempty then m else f m

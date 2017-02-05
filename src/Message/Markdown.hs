{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}

module Message.Markdown
    ( quote
    , ml
    , mr
    , ma
    , tl
    , nl

    , italic
    , bold
    , codeblock
    , blockquote
    , link
    , h1, h2, h3, h4, h5
    , itemize

    , uln
    , cmb
    ) where

import           Data.Maybe
import           Data.Monoid
import           Data.String (IsString)

-- ----------------------------------------------

type Stringish s = (Monoid s, IsString s, Eq s)

-- | Text as a codeblock
codeblock :: Stringish m => m -> m
codeblock = ta "```"

-- | Linkified text
link :: Stringish m => m -> m -> m
link text url = nonNull (\text' -> "[" <> text' <> "](" <> url <> ")") text

-- | Text put in quotes
quote :: Stringish m => m -> m
quote = ta "\""

-- | Text in italics
italic :: Stringish m => m -> m
italic = ta "*"

-- | Text in bold
bold :: Stringish m => m -> m
bold = italic . italic

-- | Block quotes
blockquote :: Stringish m => m -> m
blockquote = tl "> "

itemize :: Stringish m => [m] -> [m]
itemize = map (tl "- ")

-- | Margin left
ml :: Stringish m => m -> m
ml = tl " "

-- | Margin right
mr :: Stringish m => m -> m
mr = tr " "

-- | Margin right
ma :: Stringish m => m -> m
ma = ta " "

tl :: Stringish m => m -> m -> m
tl prefix = nonNull $ \text -> prefix <> text

tr :: Stringish m => m -> m -> m
tr suffix = nonNull $ \text -> text <> suffix

ta :: Stringish m => m -> m -> m
ta surrfix = tl surrfix . tr surrfix

h1 :: Stringish m => m -> m
h1 = tl "# "

h2 :: Stringish m => m -> m
h2 = tl "## "

h3 :: Stringish m => m -> m
h3 = tl "### "

h4 :: Stringish m => m -> m
h4 = tl "#### "

h5 :: Stringish m => m -> m
h5 = tl "##### "

-- | Applies f if m is non-empty
nonNull :: Stringish m  => (m -> m) -> m -> m
nonNull f m = if m == mempty then m else f m

nl :: (IsString s) => s
nl = "\n"

cmb :: (Stringish s) => Maybe s -> s
cmb = fromMaybe mempty

uln :: (Stringish s) => [s] -> s
uln = intercalate (\x y-> x <> "\n" <> y)


intercalate :: Monoid s => (s -> s -> s) -> [s] -> s
intercalate f = ic
  where
  ic []     = mempty
  ic [s]    = s
  ic (s:ss) = s `f` ic ss

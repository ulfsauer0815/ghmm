{-# LANGUAGE ConstraintKinds   #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Utility functions related to message formatting, especially markdown.
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
    , ln
    , cmb
    ) where

import           Data.Maybe
import           Data.Monoid
import           Data.String (IsString)
import           Data.Text   (Text)
import qualified Data.Text   as T

-- ----------------------------------------------

-- | Type constraints for spring-like types.
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

-- | List of items.
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

-- | Text left.
tl :: Stringish m => m -> m -> m
tl prefix = nonNull $ \text -> prefix <> text

-- | Text right.
tr :: Stringish m => m -> m -> m
tr suffix = nonNull $ \text -> text <> suffix

-- | Surrounding text.
ta :: Stringish m => m -> m -> m
ta surrfix = tl surrfix . tr surrfix

-- | Headline h1.
h1 :: Stringish m => m -> m
h1 = tl "# "

-- | Headline h2.
h2 :: Stringish m => m -> m
h2 = tl "## "

-- | Headline h3.
h3 :: Stringish m => m -> m
h3 = tl "### "

-- | Headline h4.
h4 :: Stringish m => m -> m
h4 = tl "#### "

-- | Headline h5.
h5 :: Stringish m => m -> m
h5 = tl "##### "

-- | Applies f if m is non-empty
nonNull :: Stringish m  => (m -> m) -> m -> m
nonNull f m = if m == mempty then m else f m

-- | New line.
nl :: (IsString s) => s
nl = "\n"

-- | 'Maybe' to 'Stringish'. 'Nothing' is the empty string / 'mempty'.
cmb :: (Stringish s) => Maybe s -> s
cmb = fromMaybe mempty

-- | 'unlines' with the newline \'\\n\' character.
uln :: (Stringish s) => [s] -> s
uln = intercalate (\x y-> x <> "\n" <> y)

ln :: Text -> [Text]
ln = T.split (== '\n')

-- | 'Data.Text.intercalate' for 'Monoid's.
intercalate :: Monoid s => (s -> s -> s) -> [s] -> s
intercalate f = ic
  where
  ic []     = mempty
  ic [s]    = s
  ic (s:ss) = s `f` ic ss

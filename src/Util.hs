-- | General utility functions.
module Util
  ( eitherToMaybe
  , fromRight
  ) where

-- | 'Either' to 'Maybe' discarding the 'Left' value.
eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe = either (const Nothing) Just

-- | Return the 'Right' value or crash.
fromRight :: Either b a -> a
fromRight (Right e) = e
fromRight _         = error "Util.fromRight: Left value"

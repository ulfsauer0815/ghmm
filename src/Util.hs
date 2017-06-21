-- | General utility functions.
module Util
  ( eitherToMaybe
  , maybeToEither
  , fromRight
  ) where

-- | 'Either' to 'Maybe' discarding the 'Left' value.
eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe = either (const Nothing) Just

-- | 'Maybe' to 'Either', providing the possible 'Left' value.
maybeToEither :: b -> Maybe a -> Either b a
maybeToEither _ (Just a) = Right a
maybeToEither d Nothing  = Left d

-- | Return the 'Right' value or crash.
fromRight :: Either b a -> a
fromRight (Right e) = e
fromRight _         = error "Util.fromRight: Left value"

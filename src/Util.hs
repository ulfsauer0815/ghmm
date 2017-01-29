module Util
  ( eitherToMaybe
  , fromRight
  ) where

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe = either (const Nothing) Just

fromRight :: Either b a -> a
fromRight (Right e) = e
fromRight _ = error "Util.fromRight: Left value"

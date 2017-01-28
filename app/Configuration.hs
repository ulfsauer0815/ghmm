module Configuration
  ( env
  , read
  , envRead
  , envMaybeBS
  , withDef
  ) where

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe

import qualified Text.Read                 as TR

import           Data.ByteString           (ByteString)
import qualified Data.ByteString.Char8     as BS8
import           Data.Text                 (Text)
import qualified Data.Text                 as T

import           System.Environment

-- ----------------------------------------------

env :: Text -> MaybeT IO Text
env = env' (return . T.pack)

env' :: (String -> MaybeT IO b) -> Text -> MaybeT IO b
env' valToMaybeT key = do
  val <- lift (lookupEnv (T.unpack key)) >>= liftMaybe
  valToMaybeT val

envMaybe' :: (Maybe String -> MaybeT IO (Maybe b)) -> Text -> MaybeT IO (Maybe b)
envMaybe' valToMaybeT key = do
  val <- lift (lookupEnv (T.unpack key)) >>= (liftMaybe . return)
  valToMaybeT val

envMaybe :: Text -> MaybeT IO (Maybe Text)
envMaybe = envMaybe' (return . fmap T.pack)

envMaybeBS :: Text -> MaybeT IO (Maybe ByteString)
envMaybeBS = envMaybe' (return . fmap BS8.pack)

-- XXX: careful with readEither and non-`Read`able types
envRead :: Read b => Text -> MaybeT IO b
envRead = env' (liftMaybe . eitherToMaybe . TR.readEither)


-- ----------------------------------------------

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe = either (const Nothing) Just

withDef :: MaybeT IO a -> a -> MaybeT IO a
withDef m def = lift $ runMaybeT m >>= \val ->
    case val of
      Just v  -> return v
      Nothing -> return def

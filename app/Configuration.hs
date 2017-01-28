module Configuration
  ( runConfigReader

  , env
  , read
  , envRead
  , envBS
  , withDef
  , opt
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

runConfigReader :: MaybeT IO cfg -> IO (Maybe cfg)
runConfigReader = runMaybeT

-- ----------------------------------------------

env :: Text -> MaybeT IO Text
env = env' (return . T.pack)

env' :: (String -> MaybeT IO b) -> Text -> MaybeT IO b
env' valToMaybeT key = do
  val <- lift (lookupEnv (T.unpack key)) >>= liftMaybe
  valToMaybeT val

-- XXX: silently defaults if parsing fails
opt :: (Text -> MaybeT IO a) -> Text -> MaybeT IO (Maybe a)
opt f t = return <$> f t

envBS :: Text -> MaybeT IO ByteString
envBS = env' (return . BS8.pack)

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

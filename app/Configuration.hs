module Configuration
  ( ConfigReader
  , runConfigReader

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

type ConfigReader = MaybeT IO

runConfigReader :: ConfigReader cfg -> IO (Maybe cfg)
runConfigReader = runMaybeT

-- ----------------------------------------------

env :: Text -> ConfigReader Text
env = env' (return . T.pack)

env' :: (String -> ConfigReader b) -> Text -> ConfigReader b
env' valToMaybeT key = do
  val <- lift (lookupEnv (T.unpack key)) >>= liftMaybe
  valToMaybeT val

-- XXX: silently defaults if parsing fails
opt :: (Text -> ConfigReader a) -> Text -> ConfigReader (Maybe a)
opt f t = return <$> f t

envBS :: Text -> ConfigReader ByteString
envBS = env' (return . BS8.pack)

-- XXX: careful with readEither and non-`Read`able types
envRead :: Read b => Text -> ConfigReader b
envRead = env' (liftMaybe . eitherToMaybe . TR.readEither)

-- ----------------------------------------------

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe = either (const Nothing) Just

withDef :: ConfigReader a -> a -> ConfigReader a
withDef m def = lift $ runMaybeT m >>= \val ->
    case val of
      Just v  -> return v
      Nothing -> return def

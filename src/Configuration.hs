-- | Minimalistic configuration reader using environment variables.
--
--   Based on the 'Maybe' monad, which means that failures are represented by a
--   lonesome 'Nothing'.
module Configuration
  ( ConfigReader
  , runConfigReader

  , env
  , env'
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

import           Util

-- ----------------------------------------------

-- | The configuration reader type.
type ConfigReader = MaybeT IO

-- | Run the configuration reader.
runConfigReader :: ConfigReader cfg -> IO (Maybe cfg)
runConfigReader = runMaybeT

-- ----------------------------------------------

-- | Read an environment variable with this name.
env :: Text -> ConfigReader Text
env = env' (return . T.pack)

-- | Read an environment variable with that name
--   and convert the result with the given function.
env' :: (String -> ConfigReader b) -> Text -> ConfigReader b
env' valToMaybeT key = do
  val <- lift (lookupEnv (T.unpack key)) >>= liftMaybe
  valToMaybeT val

-- XXX: silently defaults if parsing fails
-- | Lift the reader result to a 'Maybe'.
opt :: (Text -> ConfigReader a) -> Text -> ConfigReader (Maybe a)
opt f t = return <$> f t

-- | Read an environment variable as a 'ByteString'.
envBS :: Text -> ConfigReader ByteString
envBS = env' (return . BS8.pack)

-- | Read an environment variable and convert the result to the desired type
--   using its 'Read' instance.
--
-- /Note/: Non-'Read'able types will fail. This cannot be used for conversion
--         of string types. See 'TR.readEither'.
envRead :: Read b => Text -> ConfigReader b
envRead = env' (liftMaybe . eitherToMaybe . TR.readEither)

-- ----------------------------------------------

-- | Lift me up.
liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

-- | With default combinator, e.g.
--
--  @env \"URL\" `withDef` \"www.google.com\"@
withDef :: ConfigReader a -> a -> ConfigReader a
withDef m def = lift $ runMaybeT m >>= \val ->
    case val of
      Just v  -> return v
      Nothing -> return def

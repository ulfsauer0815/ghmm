{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Network.Wai.Handler.Warp             (run)
import           Network.Wai.Middleware.RequestLogger

import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Maybe

import qualified Text.Read                            as TR

import           Data.ByteString                      (ByteString)
import qualified Data.ByteString.Char8                as BS8
import           Data.Text                            (Text)
import qualified Data.Text                            as T

import           System.Environment

import           App
import           HmacMiddleware
import           Lib

-- ----------------------------------------------
-- TODO: pimp error handling (Maybe -> Either/Except?)
main :: IO ()
main = do
  mbConfig <- runMaybeT loadConfiguration
  case mbConfig of
    Just config -> do
      let optAuthware = case cfgGithubSecret config of
                          Just secret -> hmacAuth $ defaultAuthSettings secret
                          Nothing     -> id
      let middleware = logStdoutDev . optAuthware
      run (cfgPort config) $ middleware $ app config
      startApp config
    Nothing -> putStrLn "Incomplete/invalid configuration" -- I'm just a Maybe, baby

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

loadConfiguration :: MaybeT IO Configuration
loadConfiguration =
  Configuration
    <$> envRead    "PORT"
    <*> envMaybeBS "GITHUB_SECRET"
    <*> env        "MATTERMOST_URL"
    <*> envRead    "MATTERMOST_PORT"
    <*> env        "MATTERMOST_API_KEY"

-- ----------------------------------------------

liftMaybe :: (Monad m) => Maybe a -> MaybeT m a
liftMaybe = MaybeT . return

eitherToMaybe :: Either b a -> Maybe a
eitherToMaybe = either (const Nothing) Just

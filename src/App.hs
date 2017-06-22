{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}

-- | The app, its runtime context and configuration.
module App
  ( App(..)
  , AppContext(..)
  , Config(..)
  , RepositoryConfig(..)
  , BotConfig(..)

  , cfg

  -- * Reexports from Servant.Client
  , BaseUrl
  , parseBaseUrl
  , showBaseUrl
  ) where

import           Control.Monad.Except (ExceptT, MonadError)
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT, asks)

import qualified Network.HTTP.Client  as HttpClient

import           Servant
import           Servant.Client

import           Config.Types

-- ----------------------------------------------

-- | The app's monad stack.
newtype App a = App
  { runApp :: ReaderT AppContext (ExceptT ServantErr IO) a
  } deriving ( Functor, Applicative, Monad, MonadReader AppContext,
               MonadError ServantErr, MonadIO)

-- | The context the app has access to during runtime.
data AppContext = AppContext
  { ctxConfiguration     :: Config       -- App configuration
  , ctxHttpClientManager :: HttpClient.Manager  -- HTTP Client manager to use for http calls
  }

-- | Get the configuration in app context.
cfg :: MonadReader AppContext m => (Config -> a) -> m a
cfg f = asks (f . ctxConfiguration)

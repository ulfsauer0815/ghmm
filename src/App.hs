{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module App where

import           Control.Monad.Except (ExceptT, MonadError)
import           Control.Monad.Reader (MonadIO, MonadReader, ReaderT)

import           Data.ByteString      (ByteString)
import           Data.Text            (Text)

import           Servant

-- ----------------------------------------------

newtype App a
  = App
  { runApp :: ReaderT Configuration (ExceptT ServantErr IO) a
  } deriving ( Functor, Applicative, Monad, MonadReader Configuration,
               MonadError ServantErr, MonadIO)


data Configuration = Configuration
 { cfgPort             :: Int
 , cfgGithubSecret     :: Maybe ByteString
 , cfgMattermostUrl    :: Text
 , cfgMattermostPort   :: Int
 , cfgMattermostApiKey :: Text
 }

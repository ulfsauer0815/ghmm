{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Mattermost.Api
    ( Api

    , MessagePayload(..)

    , hook
    , api
    ) where

import           Data.Text                  (Text)

import           Servant
import           Servant.Client

import           Mattermost.Types

-- ----------------------------------------------

type Api =
       "hooks" :> Capture "key" Text :> ReqBody '[JSON] MessagePayload :> Post '[JSON] NoContent

-- ----------------------------------------------

api :: Proxy Api
api = Proxy

-- hook :: Text -> MessagePayload -> Manager -> BaseUrl-> ClientM NoContent
hook :: Client Api
hook = client api

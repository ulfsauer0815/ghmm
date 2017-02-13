{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Mattermost.Api
    ( Api

    , MessagePayload(..)
    , Attachment(..)
    , Field(..)

    , hook
    , api
    ) where

import           Data.Text        (Text)

import           Servant
import           Servant.Client

import           Mattermost.Types

-- ----------------------------------------------

-- | Mattermost API type.
type Api =
       "hooks" :> Capture "key" Text :> ReqBody '[JSON] MessagePayload :> Post '[JSON] NoContent

-- ----------------------------------------------

api :: Proxy Api
api = Proxy

--hook :: Text -> MessagePayload -> ClientM NoContent
-- | Client for the Mattermost API.
hook :: Client Api
hook = client api

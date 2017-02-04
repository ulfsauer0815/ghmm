{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Mattermost.Types
    ( MessagePayload(..)
    ) where

import           GHC.Generics

import           Data.Aeson
import           Data.Text    (Text)

-- ----------------------------------------------

{-# ANN type MessagePayload ("HLint: ignore Use camelCase" :: Text) #-}

data MessagePayload = MessagePayload
 { text     :: Text
 , username :: Maybe Text
 , icon_url :: Maybe Text
 , channel  :: Maybe Text
 } deriving (Eq, Show, Generic)

instance ToJSON MessagePayload

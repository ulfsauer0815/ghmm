{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Mattermost.Types
    ( MessagePayload(..)
    , Attachment(..)
    , Field(..)

    , attachment
    ) where

import           GHC.Generics

import           Data.Aeson
import           Data.Text    (Text)

import qualified JsonOptions  as Json

-- ----------------------------------------------

{-# ANN type MessagePayload ("HLint: ignore Use camelCase" :: Text) #-}

data MessagePayload
  = MessagePayload
     { mptText        :: Maybe Text
     , mptUsername    :: Maybe Text
     , mptIcon_url    :: Maybe Text
     , mptChannel     :: Maybe Text
     , mptAttachments :: [Attachment]
     } deriving (Eq, Show, Generic)

instance ToJSON MessagePayload where
  toEncoding = genericToEncoding Json.encodingOptions


{-# ANN type Attachment ("HLint: ignore Use camelCase" :: Text) #-}

-- |  See <https://docs.mattermost.com/developer/message-attachments.html message attachment documentation>.
data Attachment = Attachment
  { attFallback    :: Maybe Text
  , attColor       :: Maybe Text
  , attPretext     :: Maybe Text
  , attText        :: Maybe Text
  , attAuthor_name :: Maybe Text
  , attAuthor_link :: Maybe Text
  , attTitle       :: Maybe Text
  , attTitle_link  :: Maybe Text
  , attFields      :: [Field]
  } deriving (Eq, Show, Generic)

instance ToJSON Attachment where
  toEncoding = genericToEncoding Json.parseOptions


data Field = Field
  { fldShort :: Bool
  , fldTitle :: Maybe Text
  , fldValue :: Maybe Text
  }
  deriving (Eq, Show, Generic)

instance ToJSON Field where
  toEncoding = genericToEncoding Json.parseOptions

-- ----------------------------------------------

attachment :: Attachment
attachment = Attachment
  { attFallback    = Nothing
  , attColor       = Nothing
  , attPretext     = Nothing
  , attText        = Nothing
  , attAuthor_name = Nothing
  , attAuthor_link = Nothing
  , attTitle       = Nothing
  , attTitle_link  = Nothing
  , attFields      = []
  }

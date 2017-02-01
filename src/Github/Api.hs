{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Github.Api
    ( Api

    , Event(..)
    , EventPayload(..)
    , Commit(..)
    , PushCommit(..)
    , Repository(..)
    , PullRequest(..)
    , Issue(..)
    , Comment(..)
    , User(..)
    , Review(..)
    ) where

import           Data.Aeson
import           Data.Text          (Text)

import           Servant

import           Github.Event.Types

-- ----------------------------------------------

type Api =
     Header "X-GitHub-Delivery" Text
  :> Header "X-GitHub-Event" Text
  :> ReqBody '[JSON] Value
  :> PostNoContent '[JSON] NoContent

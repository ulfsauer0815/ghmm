{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module Github.Api
    ( Api

    , module Github.Event.Types
    ) where

import           Data.Aeson
import           Data.Text          (Text)

import           Servant

import           Github.Event.Types

-- ----------------------------------------------

-- | The GitHub API type.
--
--   The request body is a simple Value, because proper deserialization requires
--   the additional @X-GitHub-Event@ header value.
type Api =
     Header "X-GitHub-Delivery" Text
  :> Header "X-GitHub-Event" Text
  :> ReqBody '[JSON] Value
  :> PostNoContent '[JSON] NoContent

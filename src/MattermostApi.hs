{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators     #-}

module MattermostApi
    ( MattermostApi
    , postEvent
    ) where

import           GHC.Generics

import           Control.Monad.Reader
import           Control.Monad.Trans.Except (runExceptT)

import           Data.Aeson
import           Data.Monoid
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import qualified Data.Text.IO               as T

import           Network.HTTP.Client        (newManager)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)

import           Servant
import           Servant.Client

import           App
import           GithubApi
import           MessageRendering           as Msg

-- ----------------------------------------------

type MattermostApi =
       "hooks" :> Capture "key" Text :> ReqBody '[JSON] MessagePayload :> Post '[JSON] NoContent


{-# ANN type MessagePayload ("HLint: ignore Use camelCase" :: Text) #-}

data MessagePayload = MessagePayload
 { text     :: Text
 , username :: Maybe Text
 , icon_url :: Maybe Text
 } deriving (Eq, Show, Generic)

instance ToJSON MessagePayload

-- ----------------------------------------------

mattermostApi :: Proxy MattermostApi
mattermostApi = Proxy

-- hook :: Text -> MessagePayload -> Manager -> BaseUrl-> ClientM NoContent
hook :: Client MattermostApi
hook = client mattermostApi


postEvent :: Event -> App NoContent
postEvent e = do
  mmUrl    <- asks cfgMattermostUrl
  mmPort   <- asks cfgMattermostPort
  mmApiKey <- asks cfgMattermostApiKey
  -- TODO: put into context
  res <- liftIO $ do
    manager <- liftIO $ newManager tlsManagerSettings
    T.putStrLn $ "raw message: " <> messageText
    runExceptT $ hook mmApiKey
      payload
      manager (BaseUrl Https (T.unpack mmUrl) mmPort "") -- TODO: hardcoded https?
  case res of
    Left err        -> liftIO . putStrLn $ "Error: " <> show err
    Right NoContent -> return ()
  return NoContent

  where
  messageText = renderMessageText e
  payload = MessagePayload
    { text = messageText
    , username = Just "GitHub"
    , icon_url = Just "http://i.imgur.com/NQA4pPs.png"
    }


-- TODO: proper markdown renderer, more data.text.builder
renderMessageText :: Event -> Text
renderMessageText event
  = case event of
    PushEvent ref commits headCommit compare repository ->
      "[" <> link (repName repository) (repHtml_url repository) <> "] "
       <> "Push ("
       <> optBranch ref
       <> (T.pack . show . length $ commits) <> "): "
       <> link (quote (shortenCommitMessage $ cmtMessage headCommit)) compare
    PullRequestEvent action number (PullRequest htmlUrl state title) repository ->
      repoPrefix repository
       <> link ("Pull Request #" <> (T.pack . show) number <> " - " <> state) htmlUrl
       <> " " <> italic action <> ": "
       <> title
    StatusEvent sha state description statusUrl repository ->
      repoPrefix repository
       <> link ("Status: " <> state) statusUrl
       <> ": " <> description
    CommentEvent action (Issue state issueHtmlUrl issueUser) (Comment commentHtmlUrl commentBody commentUser) repository ->
      repoPrefix repository
       <> link ("Comment " <> italic action <> " (" <> usrLogin commentUser <> ")") commentHtmlUrl
       <> ": " <> shortenCommentMessage commentBody
  where
    -- XXX: hardcoded master branch, use payload default branch data
  optBranch ref = if ref /= "refs/heads/master" then "on " <> lastSegment ref <> ", " else ""
  lastSegment = last . T.splitOn "/"
  repoPrefix repo = "[" <> link (repName repo) (repHtml_url repo) <> "] "
  shortenCommitMessage = shorten 50 "..." . firstLine
  shortenCommentMessage = shorten 72 "..." . firstLine -- XXX: cuts off multilines without marker
  firstLine = T.takeWhile (/= '\n')

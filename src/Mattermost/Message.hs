{-# LANGUAGE OverloadedStrings #-}

module Mattermost.Message
    ( renderMessage
    ) where

import           Data.Monoid
import qualified Data.Text        as T

import           Github.Api
import           Mattermost.Types
import           Message.Markdown

-- ----------------------------------------------

renderMessage :: MessagePayload -> EventPayload -> MessagePayload
renderMessage message event
  = case event of

    PushEvent ref commits headCommit compareUrl repository ->
      message
       { mptText        = Just text
       }
      where
      text =
        let commitsText = (T.pack . show $ commitsLength) <> if commitsLength == 1 then "" else "s"
            commitsLength = length commits
            branch = optBranch (repDefault_branch repository) ref
        in repoPrefix repository
             <> link "Push" compareUrl <> ": " <> commitsText <> ml branch
               <> nl <> (h5 . firstLine $ pcmMessage headCommit)

    PullRequestEvent action _ (PullRequest number htmlUrl _state title) repository ->
      message
       { mptText        = Just text
       }
      where
      text =
        let optAction = if action == "synchronize" then "sync" else action
        in repoPrefix repository
             <> link ("PR" <> (tl " #" . T.pack . show) number) htmlUrl
             <> (ml . italic) optAction
             <> nl <> h5 title

    StatusEvent _ state description (Commit sha commitUrl) targetUrl repository ->
      message
       { mptText        = Just text
       }
      where
      text =
        let htmlUrl = fromEmpty commitUrl targetUrl
        in  repoPrefix repository
             <> link ("Status (" <> shaify sha <> ")") htmlUrl <> " " <> italic state
             <> nl <> (h5 . cmb) description

    IssueCommentEvent action (Issue _state _ _) (Comment commentHtmlUrl commentBody commentUser) repository ->
      message
       { mptText        = Just text
       }
      where
      text =
        let optAction = if action == "created" then "" else action
        in  repoPrefix repository
             <> link "Comment" commentHtmlUrl <> (ml . italic) optAction <> ml "(" <> usrLogin commentUser <> ")"
             <> nl <> (blockquote . firstLine) commentBody

    PullRequestReviewEvent action (Review rvHtmlUrl rvBody _state rvUser) (PullRequest number _ _prState _title) repository ->
      message
       { mptText        = Just text
       }
      where
      text =
        repoPrefix repository
         <> link ("PR #" <> (T.pack . show) number <> " Review") rvHtmlUrl <> (ml . italic) action <> " (" <> usrLogin rvUser <> ")"
         <> nl <> (blockquote . firstLine) rvBody

    PullRequestReviewCommentEvent action (Comment commentHtmlUrl commentBody commentUser) (PullRequest number _ _state _title) repository ->
      message
       { mptText        = Just text
       }
      where
      text =
        let optAction = if action == "created" then "" else italic action
        in  repoPrefix repository
              <> link ("PR #" <> (T.pack . show) number <> ml "Review Comment") commentHtmlUrl <> (ml . italic) optAction <> ml "(" <> usrLogin commentUser <> ")"
              <> nl <> (blockquote . firstLine) commentBody

  where
  optBranch defaultBranch ref =
     if ref /= "refs/heads/" <> defaultBranch then "on " <> lastSegment ref else ""
  lastSegment = last . T.splitOn "/"
  repoPrefix repo = "[" <> link (repName repo) (repHtml_url repo) <> "] "
  firstLine = T.takeWhile (/= '\n')
  ifPresent e f = maybe mempty f e
  modifyIfPresent t eMb f = maybe t (f t) eMb
  shaify t = "#" <> T.take 7 t
  fromEmpty x (Just "") = x
  fromEmpty _ (Just y)  = y
  fromEmpty x Nothing   = x

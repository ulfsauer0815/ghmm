{-# LANGUAGE OverloadedStrings #-}

module Github.Event.Message
    ( renderMessageText
    ) where

import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as T

import           Github.Api
import           Message.Markdown

-- ----------------------------------------------

renderMessageText :: EventPayload -> Text
renderMessageText event
  = case event of
    PushEvent ref commits headCommit compareUrl repository ->
      repoPrefix repository
       <> "Push ("
       <> optBranch ref
       <> (T.pack . show . length $ commits) <> "): "
       <> link (quote (firstLine $ pcmMessage headCommit)) compareUrl
    PullRequestEvent action _ (PullRequest number htmlUrl _state title) repository ->
      let optAction = if action == "synchronize" then "sync" else action
      in repoPrefix repository
           <> link ("PR #" <> (T.pack . show) number) htmlUrl
           <> (ml . italic) optAction
           <> tl ": " title
    StatusEvent _ state description (Commit sha commitUrl) targetUrl repository ->
      let htmlUrl = fromEmpty commitUrl targetUrl
      in  repoPrefix repository
           <> link ("Status (" <> shaify sha <> ")") htmlUrl <> ": " <> italic state
           <> ifPresent description (": "  <>)
    IssueCommentEvent action (Issue _state _ _) (Comment commentHtmlUrl commentBody commentUser) repository ->
      let optAction = if action == "created" then "" else action
      in  repoPrefix repository
           <> link ("Comment" <> (ml . italic) optAction <> ml "(" <> usrLogin commentUser <> ")") commentHtmlUrl
           <> ": " <> firstLine commentBody
    PullRequestReviewEvent action (Review rvHtmlUrl rvBody _state rvUser) (PullRequest number _ _prState _title) repository ->
      repoPrefix repository
       <> link ("PR #" <> (T.pack . show) number <> " Review " <> italic action <> " (" <> usrLogin rvUser <> ")") rvHtmlUrl
       <> ": " <> firstLine rvBody
    PullRequestReviewCommentEvent action (Comment commentHtmlUrl commentBody commentUser) (PullRequest number _ _state _title) repository ->
      let optAction = if action == "created" then "" else italic action
      in  repoPrefix repository
            <> link ("PR #" <> (T.pack . show) number <> ml "Review Comment" <> (ml . italic) optAction <> ml "(" <> usrLogin commentUser <> ")") commentHtmlUrl
            <> ":" <> (ml . firstLine) commentBody
  where
    -- XXX: hardcoded master branch, use payload default branch data
  optBranch ref = if ref /= "refs/heads/master" then "on " <> lastSegment ref <> ", " else ""
  lastSegment = last . T.splitOn "/"
  repoPrefix repo = "[" <> link (repName repo) (repHtml_url repo) <> "] "
  firstLine = T.takeWhile (/= '\n')
  ifPresent e f = maybe mempty f e
  modifyIfPresent t eMb f = maybe t (f t) eMb
  shaify t = "#" <> T.take 7 t
  fromEmpty x (Just "") = x
  fromEmpty _ (Just y)  = y
  fromEmpty x Nothing   = x

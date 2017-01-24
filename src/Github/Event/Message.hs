{-# LANGUAGE OverloadedStrings #-}

module Github.Event.Message
    ( renderMessageText
    ) where

import           Data.Monoid
import           Data.Text        (Text)
import qualified Data.Text        as T

import           Github.Api
import           Message.Markdown
import           Message.Util

-- ----------------------------------------------

renderMessageText :: Event -> Text
renderMessageText event
  = case event of
    PushEvent ref commits headCommit compare repository ->
      repoPrefix repository
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

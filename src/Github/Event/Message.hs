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
    PushEvent ref commits headCommit compare repository ->
      repoPrefix repository
       <> "Push ("
       <> optBranch ref
       <> (T.pack . show . length $ commits) <> "): "
       <> link (quote (firstLine $ pcmMessage headCommit)) compare
    PullRequestEvent action _ (PullRequest number htmlUrl state title) repository ->
      repoPrefix repository
       <> link ("PR #" <> (T.pack . show) number <> " - " <> state) htmlUrl
       <> " " <> italic action <> ": "
       <> title
    StatusEvent _ state description (Commit sha commitUrl) targetUrl repository ->
      let htmlUrl = fromEmpty commitUrl targetUrl
      in  repoPrefix repository
           <> link ("Status (" <> shaify sha <> ")") htmlUrl <> ": " <> italic state
           <> ifPresent description (": "  <>)
    IssueCommentEvent action (Issue state issueHtmlUrl issueUser) (Comment commentHtmlUrl commentBody commentUser) repository ->
      repoPrefix repository
       <> link ("Comment " <> italic action <> " (" <> usrLogin commentUser <> ")") commentHtmlUrl
       <> ": " <> firstLine commentBody
    PullRequestReviewEvent action (Review rvHtmlUrl rvBody rvState rvUser) (PullRequest number prHtmlUrl prState title) repository ->
      repoPrefix repository
       <> link ("PR #" <> (T.pack . show) number <> " Review " <> italic action <> " (" <> usrLogin rvUser <> ")") rvHtmlUrl
       <> ": " <> firstLine rvBody
    PullRequestReviewCommentEvent action (Comment commentHtmlUrl commentBody commentUser) (PullRequest number htmlUrl state title) repository ->
       repoPrefix repository
        <> link ("PR #" <> (T.pack . show) number <> " Review Comment " <> italic action <> " (" <> usrLogin commentUser <> ")") commentHtmlUrl
        <> ": " <> firstLine commentBody
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

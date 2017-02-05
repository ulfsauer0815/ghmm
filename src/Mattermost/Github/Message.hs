{-# LANGUAGE OverloadedStrings #-}

module Mattermost.Github.Message
    ( renderMessage
    , renderMessage'
    ) where

import           Data.Monoid
import qualified Data.Text        as T

import           Github.Api
import           Mattermost.Types
import           Message.Markdown

-- ----------------------------------------------

renderMessage :: EventPayload -> MessagePayload
renderMessage = renderMessage' messageTemplate


renderMessage' :: MessagePayload -> EventPayload -> MessagePayload
renderMessage' message event
  = case event of

    PushEvent ref commits _headCommit compareUrl repository ->
      message
        { mptAttachments = [
            attachment
              { attPretext     = Just text
              , attText        = Just commitsList
              , attColor       = Just "#CCCCCC"
              }
          ]
        }
      where
      commitsList = uln . itemize . map pcmMessage $ commits
      text =
        let commitsText = (T.pack . show $ commitsLength) <> " commit" <> if commitsLength == 1 then "" else "s"
            commitsLength = length commits
            branch = optBranch (repDefault_branch repository) ref
        in repoPrefix repository
             <> link "Push" compareUrl <> ": " <> commitsText <> (tl " on " . codeblock) branch

    PullRequestEvent action _ (PullRequest number htmlUrl _state title) repository ->
      message
        { mptAttachments = [
            attachment
              { attPretext     = Just text
              , attText        = Just title
              , attColor       = Just "#23A2FF"
              , attFields      = [
                  Field
                    { fldShort = True
                    , fldTitle = Just "Action"
                    , fldValue = Just actionText
                    }
                ]
              }
          ]
        }
      where
      text = repoPrefix repository
                <> link ("Pull Request" <> (tl " #" . T.pack . show) number) htmlUrl
      actionText = if action == "synchronize" then "sync" else action

    StatusEvent _ state description (Commit sha commitUrl) targetUrl repository ->
      message
        { mptAttachments = [
            attachment
              { attPretext     = Just text
              , attText        = description
              , attColor       = Just $ if state == "success" then "#00FF00" else "#FF0000"
              }
          ]
        }
      where
      text =
        let htmlUrl = fromEmpty commitUrl targetUrl
        in  repoPrefix repository
             <> link ("Status (" <> shaify sha <> ")") htmlUrl

    IssueCommentEvent action (Issue _state _ _) (Comment commentHtmlUrl commentBody commentUser) repository ->
      message
        { mptAttachments = [
            attachment
              { attPretext     = Just text
              , attText        = Just $ (blockquote . firstLine) commentBody
              , attAuthor_name = Just $ usrLogin commentUser
              , attColor       = Just "#FFD9B3"
              }
          ]
        }
      where
      text =
        let optAction = if action == "created" then "" else action
        in  repoPrefix repository
             <> link "Comment" commentHtmlUrl <> (ml . italic) optAction

    PullRequestReviewEvent _action (Review rvHtmlUrl rvBody _state rvUser) (PullRequest number _ _prState title) repository ->
      message
        { mptAttachments = [
            attachment
              { attPretext     = Just text
              , attText        = Just $ (blockquote . firstLine) rvBody
              , attAuthor_name = Just $ usrLogin rvUser
              , attColor       = Just "#FFC080"
              }
          ]
        }
      where
      text =
        repoPrefix repository
         <> link ("Pull Request #" <> (T.pack . show) number <> " Review") rvHtmlUrl <> tl ": " title

    PullRequestReviewCommentEvent action (Comment commentHtmlUrl commentBody commentUser) (PullRequest number _ _state title) repository ->
      message
        { mptAttachments = [
            attachment
              { attPretext     = Just text
              , attText        = Just $ (blockquote . firstLine) commentBody
              , attAuthor_name = Just $ usrLogin commentUser
              , attColor       = Just "#FFD9B3"
              }
          ]
        }
      where
      text =
        let optAction = if action == "created" then "" else italic action
        in  repoPrefix repository
              <> link ("Pull Request #" <> (T.pack . show) number <> ml "Review Comment") commentHtmlUrl <> tl ": " title

  where
  optBranch defaultBranch ref =
     if ref /= "refs/heads/" <> defaultBranch then lastSegment ref else ""
  lastSegment = last . T.splitOn "/"
  repoPrefix repo = "[" <> link (repName repo) (repHtml_url repo) <> "] "
  firstLine = T.takeWhile (/= '\n')
  ifPresent e f = maybe mempty f e
  modifyIfPresent t eMb f = maybe t (f t) eMb
  shaify t = "#" <> T.take 7 t
  fromEmpty x (Just "") = x
  fromEmpty _ (Just y)  = y
  fromEmpty x Nothing   = x



messageTemplate :: MessagePayload
messageTemplate = MessagePayload
    { mptText         = Nothing
    , mptUsername     = Just "GitHub"
    , mptIcon_url     = Just "http://i.imgur.com/NQA4pPs.png"
    , mptChannel      = Nothing
    , mptAttachments  = []
    }

{-# LANGUAGE MultiWayIf        #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering of GitHub events as Mattermost messages.
module Mattermost.Github.Message
    ( renderMessage
    , renderMessage'
    , messageTemplate
    ) where

import           Data.Maybe
import           Data.Monoid
import qualified Data.Text              as T

import           Github.Api             hiding (repository)
import           Github.Event.Predicate
import           Mattermost.Types
import           Message.Markdown

-- ----------------------------------------------

-- | Render a GitHub 'EventPayload' as a Mattermost 'MessagePayload'.
--
--   Uses a message template for generic fields, see 'messageTemplate' and
--   'renderMessage''.
renderMessage :: EventPayload -> MessagePayload
renderMessage = renderMessage' messageTemplate

-- | Render a GitHub 'EventPayload' as a Mattermost 'MessagePayload' using the
--   supplied message template.
renderMessage' :: MessagePayload -> EventPayload -> MessagePayload
renderMessage' message event
  = case event of

    PingEvent zen _hookId repository ->
      message
        { mptAttachments = [
            attachment
              { attPretext     = Just text
              , attText        = Just zen
              , attColor       = Just "#000000"
              }
          ]
        }
      where
      text =
        repoPrefix repository
           <> "Ping"

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
      commitsList = uln . itemize . map (firstLine . pcmMessage) $ commits
      text =
        let commitsText = (T.pack . show $ commitsLength) <> " commit" <> if commitsLength == 1 then "" else "s"
            commitsLength = length commits
            branch = optBranch (repDefault_branch repository) ref
        in repoPrefix repository
             <> link "Push" compareUrl <> ": " <> commitsText <> (tl " on " . codeblock) branch

    PullRequestEvent action _ (PullRequest number htmlUrl _state title merged mergedBy prUser) repository ->
      message
        { mptAttachments = [
            attachment
              { attPretext     = Just text
              , attText        = Just title
              , attAuthor_name = Just $ usrLogin prUser
              , attColor       = Just color
              , attFields      = [
                  Field
                    { fldShort = True
                    , fldTitle = Just "Action"
                    , fldValue = Just actionText
                    }
                ]
                <> if wasJustMerged then [
                  Field
                    { fldShort = True
                    , fldTitle = Just "Merged by"
                    , fldValue = usrLogin <$> mergedBy
                    }
                    ]
                   else mempty
              }
          ]
        }
      where
      text = repoPrefix repository
                <> link ("Pull Request" <> (tl " #" . T.pack . show) number) htmlUrl
      actionText = if | wasJustMerged            -> "merged"
                      | action == "synchronized" -> "sync"
                      | otherwise -> action
      color = if | wasJustMerged           -> "#6E5494"
                 |    action == "opened"
                   || action == "reopened" -> "#23A2FF"
                 | action == "closed"      -> "#FF9999"
                 | otherwise               -> "#99D4FF"
      wasJustMerged = action == "closed" && fromMaybe False merged

    StatusEvent _ state description (StatusCommit sha commitUrl commit) targetUrl repository ->
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
             <> link ("Status (" <> shaify sha <> ")") htmlUrl <> ": " <> (firstLine . cmtMessage) commit

    IssuesEvent action (Issue number _state title body htmlUrl user _) repository ->
      message
        { mptAttachments = [
            attachment
              { attPretext     = Just text
              , attText        = Just body
              , attAuthor_name = Just $ usrLogin user
              , attColor       = Just color
              , attFields      = [
                  Field
                    { fldShort = True
                    , fldTitle = Just "Action"
                    , fldValue = Just action
                    }
                ]
              }
          ]
        }
      where
      text =
        repoPrefix repository
          <> link ("Issue #" <> (T.pack . show) number) htmlUrl <> tl ": " title
      color = if |    action == "opened"
                   || action == "reopened" -> "#CC317C"
                 | action == "closed"      -> "#6E5494"
                 | otherwise               -> "#CC7AA2"

    IssueCommentEvent action (Issue _number _state title _ _ _ _) (Comment commentHtmlUrl commentBody commentUser _) repository ->
      message
        { mptAttachments = [
            attachment
              { attPretext     = Just text
              , attText        = Just $ blockquote commentBody
              , attAuthor_name = Just $ usrLogin commentUser
              , attColor       = Just color
              }
          ]
        }
      where
      text =
        let optAction = if action == "created" then "" else action
        in  repoPrefix repository
             <> link "Comment" commentHtmlUrl <> (ml . italic) optAction <> tl ": " title
      color = if | isClosingIssueComment event -> "#6E5494"
                 | otherwise                   -> "#FFD9B3"

    PullRequestReviewEvent _action (Review rvHtmlUrl rvBody _state rvUser) (PullRequest number _ _prState title _merged _mergedBy _user) repository ->
      message
        { mptAttachments = [
            attachment
              { attPretext     = Just text
              , attText        = Just $ blockquote rvBody
              , attAuthor_name = Just $ usrLogin rvUser
              , attColor       = Just "#FFC080"
              }
          ]
        }
      where
      text =
        repoPrefix repository
         <> link ("Pull Request #" <> (T.pack . show) number <> " Review") rvHtmlUrl <> tl ": " title

    PullRequestReviewCommentEvent action (Comment commentHtmlUrl commentBody commentUser _) (PullRequest number _ _state title _merged _mergedBy _user) repository ->
      message
        { mptAttachments = [
            attachment
              { attPretext     = Just text
              , attText        = Just $ blockquote commentBody
              , attAuthor_name = Just $ usrLogin commentUser
              , attColor       = Just "#FFD9B3"
              }
          ]
        }
      where
      text =
        let optAction = if action == "created" then "" else italic action
        in  repoPrefix repository
              <> link ("Pull Request #" <> (T.pack . show) number <> ml "Review Comment") commentHtmlUrl <> (ml . italic) optAction <> tl ": " title

  where
  optBranch defaultBranch ref =
     if ref /= "refs/heads/" <> defaultBranch then lastSegment ref else ""
  lastSegment = last . T.splitOn "/"
  repoPrefix repo = "[" <> link (repName repo) (repHtml_url repo) <> "] "
  firstLine = T.takeWhile (/= '\n')
  shaify t = "#" <> T.take 7 t
  fromEmpty x (Just "") = x
  fromEmpty _ (Just y)  = y
  fromEmpty x Nothing   = x


-- | Default Mattermost message template.
messageTemplate :: MessagePayload
messageTemplate = MessagePayload
    { mptText         = Nothing
    , mptUsername     = Just "GitHub"
    , mptIcon_url     = Just "http://i.imgur.com/NQA4pPs.png"
    , mptChannel      = Nothing
    , mptAttachments  = []
    }

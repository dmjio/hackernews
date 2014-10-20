{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.HackerNews
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.HackerNews
       ( -- * Hacker News Monad
         hackerNews
         -- * API Calls
       , getItem
       , getStory
       , getComment
       , getPoll
       , getPollOpt
       , getUser
       , getJob
       , getTopStories
       , getMaxItem
       , getUpdates
         -- * Types
       , HackerNews
       , HackerNewsError (..)
       , Item      (..)
       , ItemId    (..)
       , Comment   (..)
       , CommentId (..)
       , Poll      (..)
       , PollId    (..)
       , PollOpt   (..)
       , PollOptId (..)
       , Story     (..)
       , StoryId   (..)
       , User      (..)
       , UserId    (..)
       , Job       (..)
       , JobId     (..)
       , Update    (..)
       , MaxItem
       , TopStories
       ) where

import           Web.HackerNews.Types
import           Web.HackerNews.Client (HackerNews, hackerNews, HackerNewsError(..))

------------------------------------------------------------------------------
-- | Retrieve a `Item` by `ItemId`
getItem :: ItemId -> HackerNews Item
getItem = getEndpoint

------------------------------------------------------------------------------
-- | Retrieve a `Story` by `StoryId`
getStory :: StoryId -> HackerNews Story
getStory = getEndpoint

------------------------------------------------------------------------------
-- | Retrieve a `Comment` by `CommentId`
getComment :: CommentId -> HackerNews Comment
getComment = getEndpoint

------------------------------------------------------------------------------
-- | Retrieve a `Poll` by `PollId`
getPoll :: PollId -> HackerNews Poll
getPoll = getEndpoint

------------------------------------------------------------------------------
-- | Retrieve a `PollOpt` by `PollOptId`
getPollOpt :: PollOptId -> HackerNews PollOpt
getPollOpt = getEndpoint

------------------------------------------------------------------------------
-- | Retrieve a `User` by `UserId`
getUser :: UserId -> HackerNews User
getUser = getEndpoint

------------------------------------------------------------------------------
-- | Retrieve a Job
getJob :: JobId -> HackerNews Job
getJob = getEndpoint

------------------------------------------------------------------------------
-- | Retrieve the Top Stories on Hacker News
getTopStories :: HackerNews TopStories
getTopStories = getEndpoint TopStoriesId

------------------------------------------------------------------------------
-- | Retrieve the largest ItemId
getMaxItem :: HackerNews MaxItem
getMaxItem = getEndpoint MaxItemId

------------------------------------------------------------------------------
-- | Retrieve the latest updates
getUpdates :: HackerNews Update
getUpdates = getEndpoint UpdateId


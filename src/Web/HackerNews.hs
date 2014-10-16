{-# LANGUAGE OverloadedStrings #-}
module Web.HackerNews
       ( -- * API Calls
         getItem
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

import           Network.Wreq (Options)

import           Web.HackerNews.Types
import           Web.HackerNews.Endpoint (getEndpointWith)

------------------------------------------------------------------------------
-- | Retrieve a `Item` by `ItemId` using provided `Options`
getItem :: Options -> ItemId -> IO (Maybe Item)
getItem = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve a `Story` by `StoryId` using provided `Options`
getStory :: Options -> StoryId -> IO (Maybe Story)
getStory = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve a `Comment` by `CommentId` using provided `Options`
getComment :: Options -> CommentId -> IO (Maybe Comment)
getComment = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve a `Poll` by `PollId` using provided `Options`
getPoll :: Options -> PollId -> IO (Maybe Poll)
getPoll = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve a `PollOpt` by `PollOptId` using provided `Options`
getPollOpt :: Options -> PollOptId -> IO (Maybe PollOpt)
getPollOpt = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve a `User` by `UserId` using provided `Options`
getUser :: Options -> UserId -> IO (Maybe User)
getUser = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve a Job using provided `Options`
getJob :: Options -> JobId -> IO (Maybe Job)
getJob = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve the Top Stories on Hacker News using provided `Options`
getTopStories :: Options -> IO (Maybe TopStories)
getTopStories = flip getEndpointWith TopStoriesId

------------------------------------------------------------------------------
-- | Retrieve the largest ItemId using provided `Options`
getMaxItem :: Options -> IO (Maybe MaxItem)
getMaxItem = flip getEndpointWith MaxItemId

------------------------------------------------------------------------------
-- | Retrieve the latest updates using provided `Options`
getUpdates :: Options -> IO (Maybe Update)
getUpdates = flip getEndpointWith UpdateId

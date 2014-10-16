{-# LANGUAGE OverloadedStrings #-}
module Web.HackerNews
       ( -- * API Calls
         getItem
       , getItemWith
       , getStory
       , getStoryWith
       , getComment
       , getCommentWith
       , getPoll
       , getPollWith
       , getPollOpt
       , getPollOptWith
       , getUser
       , getUserWith
       , getJob
       , getJobWith
       , getTopStories
       , getTopStoriesWith
       , getMaxItem
       , getMaxItemWith
       , getUpdates
       , getUpdatesWith
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

------------------------------------------------------------------------------
-- | Retrieve a `Item` by `ItemId`
getItem :: ItemId -> IO (Maybe Item)
getItem = getEndpoint

-- | Retrieve a `Item` by `ItemId` using provided `Options`
getItemWith :: Options -> ItemId -> IO (Maybe Item)
getItemWith = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve a `Story` by `StoryId`
getStory :: StoryId -> IO (Maybe Story)
getStory = getEndpoint

-- | Retrieve a `Story` by `StoryId` using provided `Options`
getStoryWith :: Options -> StoryId -> IO (Maybe Story)
getStoryWith = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve a `Comment` by `CommentId`
getComment :: CommentId -> IO (Maybe Comment)
getComment = getEndpoint

-- | Retrieve a `Comment` by `CommentId` using provided `Options`
getCommentWith :: Options -> CommentId -> IO (Maybe Comment)
getCommentWith = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve a `Poll` by `PollId`
getPoll :: PollId -> IO (Maybe Poll)
getPoll = getEndpoint

-- | Retrieve a `Poll` by `PollId` using provided `Options`
getPollWith :: Options -> PollId -> IO (Maybe Poll)
getPollWith = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve a `PollOpt` by `PollOptId`
getPollOpt :: PollOptId -> IO (Maybe PollOpt)
getPollOpt = getEndpoint

-- | Retrieve a `PollOpt` by `PollOptId` using provided `Options`
getPollOptWith :: Options -> PollOptId -> IO (Maybe PollOpt)
getPollOptWith = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve a `User` by `UserId`
getUser :: UserId -> IO (Maybe User)
getUser = getEndpoint

-- | Retrieve a `User` by `UserId` using provided `Options`
getUserWith :: Options -> UserId -> IO (Maybe User)
getUserWith = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve a Job
getJob :: JobId -> IO (Maybe Job)
getJob = getEndpoint

-- | Retrieve a Job using provided `Options`
getJobWith :: Options -> JobId -> IO (Maybe Job)
getJobWith = getEndpointWith

------------------------------------------------------------------------------
-- | Retrieve the Top Stories on Hacker News
getTopStories :: IO (Maybe TopStories)
getTopStories = getEndpoint TopStoriesId

-- | Retrieve the Top Stories on Hacker News using provided `Options`
getTopStoriesWith :: Options -> IO (Maybe TopStories)
getTopStoriesWith = flip getEndpointWith TopStoriesId

------------------------------------------------------------------------------
-- | Retrieve the largest ItemId
getMaxItem :: IO (Maybe MaxItem)
getMaxItem = getEndpoint MaxItemId

-- | Retrieve the largest ItemId using provided `Options`
getMaxItemWith :: Options -> IO (Maybe MaxItem)
getMaxItemWith = flip getEndpointWith MaxItemId

------------------------------------------------------------------------------
-- | Retrieve the latest updates
getUpdates :: IO (Maybe Update)
getUpdates = getEndpoint UpdateId

-- | Retrieve the latest updates using provided `Options`
getUpdatesWith :: Options -> IO (Maybe Update)
getUpdatesWith = flip getEndpointWith UpdateId

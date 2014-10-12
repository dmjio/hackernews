{-# LANGUAGE OverloadedStrings #-}
module Web.HackerNews
       ( -- * Hacker News Monad
         hackerNews
         -- * API Calls
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

import           Data.Monoid                ((<>))

import           Web.HackerNews.Types
import           Web.HackerNews.Util        (toText)
import           Web.HackerNews.Client

------------------------------------------------------------------------------
-- | Retrieve a `Story` by `StoryId`
getStory :: StoryId -> HackerNews (Maybe Story)
getStory (StoryId storyid) = buildHNRequest $ "item/" <> toText storyid

------------------------------------------------------------------------------
-- | Retrieve a `Comment` by `CommentId`
getComment :: CommentId -> HackerNews (Maybe Comment)
getComment (CommentId commentid) = buildHNRequest $ "item/" <> toText commentid

------------------------------------------------------------------------------
-- | Retrieve a `Poll` by `PollId`
getPoll :: PollId -> HackerNews (Maybe Poll)
getPoll (PollId pollid) = buildHNRequest $ "item/" <> toText pollid

------------------------------------------------------------------------------
-- | Retrieve a `PollOpt` by `PollOptId`
getPollOpt :: PollOptId -> HackerNews (Maybe PollOpt)
getPollOpt (PollOptId polloptid) = buildHNRequest $ "item/" <> toText polloptid

------------------------------------------------------------------------------
-- | Retrieve a `User` by `UserId`
getUser :: UserId -> HackerNews (Maybe User)
getUser (UserId userid) = buildHNRequest $ "user/" <> userid

------------------------------------------------------------------------------
-- | Retrieve a Job
getJob :: JobId -> HackerNews (Maybe Job)
getJob (JobId jobid) = buildHNRequest $ "item/" <> toText jobid

------------------------------------------------------------------------------
-- | Retrieve the Top Stories on Hacker News
getTopStories :: HackerNews (Maybe TopStories)
getTopStories = buildHNRequest "topstories"

------------------------------------------------------------------------------
-- | Retrieve the largest ItemId
getMaxItem :: HackerNews (Maybe MaxItem)
getMaxItem = buildHNRequest "maxitem"

------------------------------------------------------------------------------
-- | Retrieve the latest updates
getUpdates :: HackerNews (Maybe Update)
getUpdates = buildHNRequest "updates"


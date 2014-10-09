{-# LANGUAGE OverloadedStrings #-}
module Web.HackerNews
       ( -- * API Calls
         getStory
       , getComment
       , getPoll
       , getPollOpt
       , getUser
       , getTopStories
       , getMaxItem
       , getUpdates
       -- * Types
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
       , Update    (..)
       , MaxItem
       , TopStories
       ) where

import           Data.Monoid                ((<>))

import           Web.HackerNews.Types
import           Web.HackerNews.Util        (toText)
import           Web.HackerNews.Client      (getItem)

------------------------------------------------------------------------------
-- | Retrieve a `Story` by `StoryId`
getStory :: StoryId -> IO (Maybe Story)
getStory (StoryId storyid) = getItem $ "item/" <> toText storyid

------------------------------------------------------------------------------
-- | Retrieve a `Comment` by `CommentId`
getComment :: CommentId -> IO (Maybe Comment)
getComment (CommentId commentid) = getItem $ "item/" <> toText commentid

------------------------------------------------------------------------------
-- | Retrieve a `Poll` by `PollId`
getPoll :: PollId -> IO (Maybe Poll)
getPoll (PollId pollid) = getItem $ "item/" <> toText pollid

------------------------------------------------------------------------------
-- | Retrieve a `PollOpt` by `PollOptId`
getPollOpt :: PollOptId -> IO (Maybe PollOpt)
getPollOpt (PollOptId polloptid) = getItem $ "item/" <> toText polloptid

------------------------------------------------------------------------------
-- | Retrieve a `User` by `UserId`
getUser :: UserId -> IO (Maybe User)
getUser (UserId userid) = getItem $ "user/" <> userid

------------------------------------------------------------------------------
-- | Retrieve the Top Stories on Hacker News
getTopStories :: IO (Maybe TopStories)
getTopStories = getItem "topstories"

------------------------------------------------------------------------------
-- | Retrieve the largest ItemId
getMaxItem :: IO (Maybe MaxItem)
getMaxItem = getItem "maxitem"

------------------------------------------------------------------------------
-- | Retrieve the latest updates
getUpdates :: IO (Maybe Update)
getUpdates = getItem "updates"


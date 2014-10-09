{-# LANGUAGE OverloadedStrings #-}
module Web.HackerNews.Types
       ( module H
       , StoryId   (..)
       , CommentId (..)
       , PollId    (..)
       , PollOptId (..)
       , UserId    (..)
       , MaxItem
       , TopStories
       ) where

import           Web.HackerNews.Comment as H
import           Web.HackerNews.Poll    as H
import           Web.HackerNews.Story   as H
import           Web.HackerNews.User    as H
import           Web.HackerNews.Update  as H

type MaxItem    = Int
type TopStories = [Int]





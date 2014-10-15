{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.HackerNews.Story
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.HackerNews.Story where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson              (FromJSON (parseJSON), Value (Object),
                                          (.!=), (.:), (.:?))
import           Data.Text               (Text)
import           Data.Time               (UTCTime)

import           Web.HackerNews.Endpoint (Endpoint (endpoint), itemEndpoint)
import           Web.HackerNews.Util     (fromSeconds)

------------------------------------------------------------------------------
-- | Story Object
data Story = Story {
    storyBy      :: Text
  , storyId      :: StoryId
  , storyKids    :: [Int]
  , storyScore   :: Int
  , storyTime    :: UTCTime
  , storyTitle   :: Text
  , storyType    :: Text
  , storyUrl     :: Text
  , storyDeleted :: Bool
  } deriving Show

------------------------------------------------------------------------------
-- | ID for a `Story`
newtype StoryId
  = StoryId Int
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `Story` ID
data TopStoriesId  = TopStoriesId deriving (Show, Eq)

------------------------------------------------------------------------------
-- | TopStories List
newtype TopStories = TopStories [Int] deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint Instances for `StoryID` & `Story`
instance Endpoint StoryId Story where
    endpoint (StoryId id') = itemEndpoint id'

------------------------------------------------------------------------------
-- | Endpoint Instances for `TopStoriesID` & `TopStories`
instance Endpoint TopStoriesId TopStories where
    endpoint _ = "topstories"

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON Story where
   parseJSON (Object o) =
     Story <$> o .: "by"
           <*> (StoryId <$> o .: "id")
           <*> o .: "kids"
           <*> o .: "score"
           <*> (fromSeconds <$> o .: "time")
           <*> o .: "title"
           <*> o .: "type"
           <*> o .: "url"
           <*> o .:? "deleted" .!= False
   parseJSON _ = mzero

------------------------------------------------------------------------------
-- | JSON instances `TopStories`
instance FromJSON TopStories where
   parseJSON = fmap TopStories . parseJSON


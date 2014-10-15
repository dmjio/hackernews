{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Web.HackerNews.Story where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (MonadPlus (mzero))

import           Data.Aeson          (FromJSON (parseJSON), Value (Object),
                                      (.:), (.!=), (.:?))
import           Data.Text           (Text)
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))

import           Web.HackerNews.Util (fromSeconds, toText)
import           Web.HackerNews.Endpoint (Endpoint(endpoint))

------------------------------------------------------------------------------
-- | Types
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

newtype StoryId
  = StoryId Int
  deriving (Show, Eq)

data TopStoriesId  = TopStoriesId deriving (Show, Eq)
newtype TopStories = TopStories [Int] deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint Instances
instance Endpoint StoryId Story where
    endpoint (StoryId storId) = "item/" <> toText storId

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

instance FromJSON TopStories where
   parseJSON = fmap TopStories . parseJSON


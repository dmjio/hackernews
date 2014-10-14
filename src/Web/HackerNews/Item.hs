{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Web.HackerNews.Item where

import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson          (FromJSON (parseJSON), Value (Object), (.:))
import           Data.Text           (Text)
import           Data.Monoid         ((<>))
import           Control.Applicative
import           Web.HackerNews.Comment (Comment)
import           Web.HackerNews.Poll (Poll, PollOpt)
import           Web.HackerNews.Story (Story)
import           Web.HackerNews.Job   (Job)
import           Web.HackerNews.Endpoint (Endpoint(..))
import           Web.HackerNews.Util (toText)

newtype ItemId = ItemId Int deriving (Show,Eq)

data Item = ItemComment Comment
          | ItemPoll Poll
          | ItemPollOpt PollOpt
          | ItemStory Story
          | ItemJob Job
          deriving (Show)

instance Endpoint ItemId Item where
    endpoint (ItemId itemId) = "item/" <> toText itemId

instance FromJSON Item where
    parseJSON v@(Object o) = do
        itemType <- o .: "type"
        case (itemType :: Text) of
            "job"     -> ItemJob     <$> parseJSON v
            "story"   -> ItemStory   <$> parseJSON v
            "comment" -> ItemComment <$> parseJSON v
            "poll"    -> ItemPoll    <$> parseJSON v
            "pollopt" -> ItemPollOpt <$> parseJSON v
            _         -> mzero
    parseJSON _ = mzero

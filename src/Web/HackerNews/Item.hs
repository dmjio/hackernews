{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.Stripe.Stripe
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.HackerNews.Item where

import           Control.Applicative     ((<$>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson              (FromJSON (parseJSON), Value (Object),
                                          (.:))
import           Data.Text               (Text)

import           Web.HackerNews.Comment  (Comment)
import           Web.HackerNews.Endpoint (Endpoint (endpoint), itemEndpoint)
import           Web.HackerNews.Job      (Job)
import           Web.HackerNews.Poll     (Poll, PollOpt)
import           Web.HackerNews.Story    (Story)

------------------------------------------------------------------------------
-- | Item Type
data Item = ItemComment Comment
          | ItemPoll Poll
          | ItemPollOpt PollOpt
          | ItemStory Story
          | ItemJob Job
          deriving (Show)

------------------------------------------------------------------------------
-- | Item ID for a `Item` object
newtype ItemId = ItemId Int deriving (Show,Eq)

------------------------------------------------------------------------------
-- | Max Item ID for a `Item`
data MaxItemId = MaxItemId deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Max Item Int
newtype MaxItem = MaxItem Int deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint Instances for `MaxItemId` and `MaxItem`
instance Endpoint MaxItemId MaxItem where
    endpoint _ = "maxitem"

------------------------------------------------------------------------------
-- | Endpoint Instances for `ItemId` & `Item`
instance Endpoint ItemId Item where
    endpoint (ItemId id') = itemEndpoint id'

------------------------------------------------------------------------------
-- | JSON Instances
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

------------------------------------------------------------------------------
-- | JSON MaxItem Instance
instance FromJSON MaxItem where
    parseJSON = fmap MaxItem . parseJSON


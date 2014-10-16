{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.HackerNews.Comment
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.HackerNews.Comment where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson              (FromJSON (parseJSON), Value (Object),
                                          (.!=), (.:), (.:?))
import           Data.Text               (Text)
import           Data.Time               (UTCTime)

import           Web.HackerNews.Endpoint (Endpoint (endpoint), itemEndpoint)
import           Web.HackerNews.Util     (fromSeconds)

------------------------------------------------------------------------------
-- | Comment Object
data Comment = Comment {
    commentBy      :: Text
  , commentId      :: CommentId
  , commentKids    :: Maybe [Int]
  , commentParent  :: Int
  , commentText    :: Text
  , commentTime    :: UTCTime
  , commentType    :: Text
  , commentDeleted :: Bool
  , commentDead    :: Bool
  } deriving Show

------------------------------------------------------------------------------
-- | `CommentId` for a `Comment` Object
newtype CommentId
  = CommentId Int
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint instances
instance Endpoint CommentId Comment where
    endpoint (CommentId id') = itemEndpoint id'

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON Comment where
  parseJSON (Object o) =
     Comment <$> o .: "by"
             <*> (CommentId <$> o .: "id")
             <*> o .:? "kids"
             <*> o .: "parent"
             <*> o .: "text"
             <*> (fromSeconds <$> o .: "time")
             <*> o .: "type"
             <*> o .:? "deleted" .!= False
             <*> o .:? "dead" .!= False
  parseJSON _ = mzero




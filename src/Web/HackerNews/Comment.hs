{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Web.HackerNews.Comment where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (MonadPlus (mzero))

import           Data.Aeson          (FromJSON (parseJSON), Value (Object),
                                      (.:), (.:?), (.!=))
import           Data.Text           (Text)
import           Data.Time           (UTCTime)

import           Web.HackerNews.Util (fromSeconds)
import           Web.HackerNews.Endpoint (Endpoint(endpoint), itemEndpoint)

------------------------------------------------------------------------------
-- | Types
data Comment = Comment {
    commentBy     :: Text
  , commentId     :: CommentId
  , commentKids   :: Maybe [Int]
  , commentParent :: Int
  , commentText   :: Text
  , commentTime   :: UTCTime
  , commentType   :: Text
  , commentDeleted :: Bool
  } deriving Show

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
  parseJSON _ = mzero




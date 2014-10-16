{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.HackerNews.Job
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.HackerNews.Job where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson              (FromJSON (parseJSON), Value (Object),
                                          (.!=), (.:), (.:?))
import           Data.Text               (Text)
import           Data.Time               (UTCTime)

import           Web.HackerNews.Endpoint (Endpoint (endpoint), itemEndpoint)
import           Web.HackerNews.Util     (fromSeconds)

------------------------------------------------------------------------------
-- | Types
data Job = Job {
    jobBy      :: Text
  , jobId      :: JobId
  , jobScore   :: Int
  , jobText    :: Text
  , jobTime    :: UTCTime
  , jobTitle   :: Text
  , jobType    :: Text
  , jobUrl     :: Text
  , jobDeleted :: Bool
  , jobDead    :: Bool
  } deriving (Show)

------------------------------------------------------------------------------
-- | ID for a `Job` type
newtype JobId
      = JobId Int
      deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint Instances
instance Endpoint JobId Job where
    endpoint (JobId id') = itemEndpoint id'

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON Job where
  parseJSON (Object o) =
      Job <$> o .: "by"
          <*> (JobId <$> o .: "id")
          <*> o .: "score"
          <*> o .: "text"
          <*> (fromSeconds <$> o .: "time")
          <*> o .: "title"
          <*> o .: "type"
          <*> o .: "url"
          <*> o .:? "deleted" .!= False
          <*> o .:? "dead" .!= False
  parseJSON _ = mzero


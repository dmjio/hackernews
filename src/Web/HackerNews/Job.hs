{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Web.HackerNews.Job where

import           Control.Applicative ((<*>), (<$>))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson          (FromJSON (parseJSON), Value (Object),
                                      (.:), (.:?), (.!=))
import           Data.Text           (Text)
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))

import           Web.HackerNews.Util (fromSeconds, toText)
import           Web.HackerNews.Endpoint (Endpoint(..))

------------------------------------------------------------------------------
-- | Types
data Job = Job {
    jobBy        :: Text
  , jobId        :: JobId
  , jobScore     :: Int
  , jobText      :: Text
  , jobTime      :: UTCTime
  , jobTitle     :: Text
  , jobType      :: Text
  , jobUrl       :: Text
  , jobDeleted   :: Bool
  } deriving (Show)

newtype JobId
      = JobId Int
      deriving (Show, Eq)
------------------------------------------------------------------------------
-- | Endpoint Instances
instance Endpoint JobId Job where
    endpoint (JobId id') = "item/" <> toText id'

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
  parseJSON _ = mzero


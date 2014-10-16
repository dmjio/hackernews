{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.HackerNews.Poll
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.HackerNews.Poll where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson              (FromJSON (parseJSON), Value (Object),
                                          (.!=), (.:), (.:?))
import           Data.Text               (Text)
import           Data.Time               (UTCTime)

import           Web.HackerNews.Endpoint (Endpoint (endpoint), itemEndpoint)
import           Web.HackerNews.Util     (fromSeconds)

------------------------------------------------------------------------------
-- | Poll Object
data Poll = Poll {
    pollBy      :: Text
  , pollId      :: PollId
  , pollKids    :: [Int]
  , pollParts   :: [Int]
  , pollScore   :: Int
  , pollText    :: Text
  , pollTime    :: UTCTime
  , pollTitle   :: Text
  , pollType    :: Text
  , pollDeleted :: Bool
  , pollDead :: Bool
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Poll Opt Object
data PollOpt = PollOpt {
    pollOptBy      :: Text
  , pollOptId      :: PollOptId
  , pollOptParent  :: Int
  , pollOptScore   :: Int
  , pollOptText    :: Text
  , pollOptTime    :: UTCTime
  , pollOptType    :: Text
  , pollOptDeleted :: Bool
  , pollOptDead       :: Bool  
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Poll Option Id for a `PollOpt`
newtype PollOptId
  = PollOptId Int
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Poll Id for a `Poll`
newtype PollId
  = PollId Int
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | `Endpoint` Instance for `PollOptId`
instance Endpoint PollOptId PollOpt where
    endpoint (PollOptId id') = itemEndpoint id'

------------------------------------------------------------------------------
-- | `Endpoint` Instance for `PollId`
instance Endpoint PollId Poll where
    endpoint (PollId id') = itemEndpoint id'

------------------------------------------------------------------------------
-- | Poll JSON Instances
instance FromJSON Poll where
  parseJSON (Object o) =
     Poll <$> o .: "by"
          <*> (PollId <$> o .: "id")
          <*> o .: "kids"
          <*> o .: "parts"
          <*> o .: "score"
          <*> o .: "text"
          <*> (fromSeconds <$> o .: "time")
          <*> o .: "title"
          <*> o .: "type"
          <*> o .:? "deleted" .!= False
          <*> o .:? "dead" .!= False          
  parseJSON _ = mzero

------------------------------------------------------------------------------
-- | Poll JSON Instances
instance FromJSON PollOpt where
  parseJSON (Object o) =
     PollOpt <$> o .: "by"
             <*> (PollOptId <$> o .: "id")
             <*> o .: "parent"
             <*> o .: "score"
             <*> o .: "text"
             <*> (fromSeconds <$> o .: "time")
             <*> o .: "type"
             <*> o .:? "deleted" .!= False
             <*> o .:? "dead" .!= False             
  parseJSON _ = mzero

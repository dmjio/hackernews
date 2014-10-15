{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Web.HackerNews.Poll where

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
  } deriving (Show, Eq)

data PollOpt = PollOpt {
    pollOptBy      :: Text
  , pollOptId      :: PollOptId
  , pollOptParent  :: Int
  , pollOptScore   :: Int
  , pollOptText    :: Text
  , pollOptTime    :: UTCTime
  , pollOptType    :: Text
  , pollOptDeleted :: Bool
  } deriving (Show, Eq)

newtype PollOptId
  = PollOptId Int
  deriving (Show, Eq)

newtype PollId
  = PollId Int
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint Instances
instance Endpoint PollOptId PollOpt where
    endpoint (PollOptId pollOptId) = "item/" <> toText pollOptId

instance Endpoint PollId Poll where
    endpoint (PollId pollOptId) = "item/" <> toText pollOptId

------------------------------------------------------------------------------
-- | JSON Instances
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
  parseJSON _ = mzero

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
  parseJSON _ = mzero

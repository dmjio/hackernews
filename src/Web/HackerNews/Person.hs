{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.HackerNews.Person
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.HackerNews.Person where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson          (FromJSON (parseJSON), Value (Object),
                                      (.!=), (.:), (.:?))
import           Data.Text           (Text)
import           Data.Time           (UTCTime)

import           Web.HackerNews.Util (fromSeconds)

------------------------------------------------------------------------------
-- | Person Object
data Person = Person {
    personBy      :: Text
  , personId      :: PersonId
  , personKids    :: Maybe [Int]
  , personScore   :: Maybe Int
  , personTime    :: UTCTime
  , personTitle   :: Maybe Text
  , personType    :: Text
  , personUrl     :: Maybe Text
  , personDeleted :: Bool
  , personDead    :: Bool
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Person ID for a `Person` Object
newtype PersonId
  = PersonId Text
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON Person where
   parseJSON (Object o) =
     Person <$> o .: "by"
            <*> (PersonId <$> o .: "id")
            <*> o .:? "kids"
            <*> o .:? "score"
            <*> (fromSeconds <$> o .: "time")
            <*> o .:? "title"
            <*> o .: "type"
            <*> o .:? "url"
            <*> o .:? "deleted" .!= False
            <*> o .:? "dead" .!= False
   parseJSON _ = mzero

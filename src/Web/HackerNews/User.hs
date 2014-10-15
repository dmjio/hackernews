{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.HackerNews.User
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.HackerNews.User where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson              (FromJSON (parseJSON), Value (Object),
                                          (.!=), (.:), (.:?))
import           Data.Monoid             ((<>))
import           Data.Text               (Text)
import           Data.Time               (UTCTime)

import           Web.HackerNews.Endpoint (Endpoint (endpoint))
import           Web.HackerNews.Util     (fromSeconds, monoidToMaybe)

------------------------------------------------------------------------------
-- | `User` Object
data User = User {
    userAbout     :: Maybe Text
  , userCreated   :: UTCTime
  , userDelay     :: Int
  , userId        :: UserId
  , userKarma     :: Int
  , userSubmitted :: [Int]
  , userDeleted   :: Bool
  } deriving (Show)

------------------------------------------------------------------------------
-- | User ID for a `User` Object
newtype UserId
      = UserId Text
      deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint instances
instance Endpoint UserId User where
    endpoint (UserId id') = "user/" <> id'

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON User where
  parseJSON (Object o) =
     User <$> ((monoidToMaybe =<<) <$> (o .:? "about"))
          <*> (fromSeconds <$> o .: "created")
          <*> o .: "delay"
          <*> (UserId <$> o .: "id")
          <*> o .: "karma"
          <*> o .: "submitted"
          <*> o .:? "deleted" .!= False
  parseJSON _ = mzero


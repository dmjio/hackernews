{-# LANGUAGE OverloadedStrings #-}
module Web.HackerNews.User where

import           Control.Applicative ((<*>), (<$>))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson          (FromJSON (parseJSON), Value (Object),
                                      (.:), (.:?), (.!=))
import           Data.Text           (Text)
import           Data.Time           (UTCTime)

import           Web.HackerNews.Util (fromSeconds)

------------------------------------------------------------------------------
-- | Types
data User = User {
    userAbout     :: Maybe Text
  , userCreated   :: UTCTime
  , userDelay     :: Int
  , userId        :: UserId
  , userKarma     :: Int
  , userSubmitted :: [Int]
  , userDeleted   :: Bool
  } deriving (Show)

newtype UserId
      = UserId Text
      deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON User where
  parseJSON (Object o) =
     User <$> o .:? "about"
          <*> (fromSeconds <$> o .: "created")
          <*> o .: "delay"
          <*> (UserId <$> o .: "id")
          <*> o .: "karma"
          <*> o .: "submitted"
          <*> o .:? "deleted" .!= False
  parseJSON _ = mzero


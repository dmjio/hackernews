{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Web.HackerNews.User where

import           Control.Applicative ((<*>), (<$>))
import           Control.Monad       (MonadPlus (mzero))

import           Data.Aeson          (FromJSON (parseJSON), Value (Object),
                                      (.:), (.:?), (.!=))
import           Data.Text           (Text)
import           Data.Time           (UTCTime)
import           Data.Monoid         ((<>))

import           Web.HackerNews.Util (fromSeconds, monoidToMaybe)
import           Web.HackerNews.Endpoint (Endpoint(endpoint))

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
instance Endpoint UserId User where
    endpoint (UserId userId) = "user/" <> userId

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


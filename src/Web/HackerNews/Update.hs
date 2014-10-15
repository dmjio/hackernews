{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses #-}
module Web.HackerNews.Update where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (MonadPlus (mzero))

import           Data.Aeson          (FromJSON (parseJSON), Value (Object),
                                      (.:), (.!=), (.:?))
import           Data.Text           (Text)

import           Web.HackerNews.Endpoint (Endpoint(endpoint))

------------------------------------------------------------------------------
-- | Types
data Update = Update {
    updateItems    :: [Int]
  , updateProfiles :: [Text]
  , updateDeleted  :: Bool
  } deriving (Show, Eq)

data UpdateId = UpdateId deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Endpoint Instances
instance Endpoint UpdateId Update where
    endpoint _ = "updates"

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON Update where
  parseJSON (Object o) =
     Update <$> o .: "items"
            <*> o .: "profiles"
            <*> o .:? "deleted" .!= False
  parseJSON _ = mzero


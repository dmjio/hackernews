{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
-- |
-- Module      : Web.HackerNews.Update
-- Copyright   : (c) David Johnson, Konstantin Zudov, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.HackerNews.Update where

import           Control.Applicative     ((<$>), (<*>))
import           Control.Monad           (MonadPlus (mzero))
import           Data.Aeson              (FromJSON (parseJSON), Value (Object),
                                          (.!=), (.:), (.:?))
import           Data.Text               (Text)

import           Web.HackerNews.Endpoint (Endpoint (endpoint))

------------------------------------------------------------------------------
-- | Update Object
data Update = Update {
    updateItems    :: [Int]
  , updateProfiles :: [Text]
  , updateDeleted  :: Bool
  , updateDead     :: Bool
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | Update ID for an `Updated` Object
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
            <*> o .:? "dead" .!= False
  parseJSON _ = mzero


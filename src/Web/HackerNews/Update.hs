{-# LANGUAGE OverloadedStrings #-}
module Web.HackerNews.Update where

import           Control.Applicative ((<$>), (<*>))
import           Control.Monad       (MonadPlus (mzero))
import           Data.Aeson          (FromJSON (parseJSON), Value (Object),
                                      (.:), (.!=), (.:?))
import           Data.Text           (Text)

------------------------------------------------------------------------------
-- | Types
data Update = Update {
    updateItems    :: [Int]
  , updateProfiles :: [Text]
  , updateDeleted  :: Bool
  } deriving (Show, Eq)

------------------------------------------------------------------------------
-- | JSON Instances
instance FromJSON Update where
  parseJSON (Object o) =
     Update <$> o .: "items"
            <*> o .: "profiles"
            <*> o .:? "deleted" .!= False
  parseJSON _ = mzero


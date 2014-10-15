-- |
-- Module      : Web.HackerNews.Util
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.HackerNews.Util where

import           Data.Monoid           (Monoid, mempty)
import           Data.Text             (Text)
import qualified Data.Text             as T
import           Data.Time             (UTCTime)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)

------------------------------------------------------------------------------
-- | Convert `Integer` to `UTCTime`
fromSeconds :: Integer -> UTCTime
fromSeconds = posixSecondsToUTCTime . fromInteger

------------------------------------------------------------------------------
-- | Convert `Show` constrained a to `Text`
toText :: Show a => a -> Text
toText = T.pack . show

------------------------------------------------------------------------------
-- | Turns empty monoids into Nothing
-- `listToMaybe` generalized for monoids
monoidToMaybe :: (Eq a, Monoid a) => a -> Maybe a
monoidToMaybe m = if m == mempty then Nothing else Just m

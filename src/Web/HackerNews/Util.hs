module Web.HackerNews.Util where

import qualified Data.Text as T
import           Data.Text    (Text)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time (UTCTime)
import           Data.Monoid (Monoid, mempty)

------------------------------------------------------------------------------
-- | Convert `Integer` to `UTCTime`
fromSeconds :: Integer -> UTCTime
fromSeconds = posixSecondsToUTCTime . fromInteger

------------------------------------------------------------------------------
-- | Convert `Show` constrained a to `Text`
toText :: Show a => a -> Text
toText = T.pack . show

-- | Turns empty monoids into Nothing
-- | @Data.Maybe.listToMaybe@ generalized for monoids
monoidToMaybe :: (Eq a, Monoid a) => a -> Maybe a
monoidToMaybe m = if m == mempty then Nothing else Just m

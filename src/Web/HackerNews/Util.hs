module Web.HackerNews.Util where

import qualified Data.Text as T
import           Data.Text    (Text)
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time (UTCTime)

------------------------------------------------------------------------------
-- | Convert `Integer` to `UTCTime`
fromSeconds :: Integer -> UTCTime
fromSeconds = posixSecondsToUTCTime . fromInteger

------------------------------------------------------------------------------
-- | Convert `Show` constrained a to `Text`
toText :: Show a => a -> Text
toText = T.pack . show



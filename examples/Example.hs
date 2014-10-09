module Example where

import           Control.Monad  (liftM3)
import           Web.HackerNews (UserId (..), getUser, hackerNews)

main :: IO ()
main = print =<< hackerNews (liftM3 (,,) one two three)
  where
    one   = getUser (UserId "dmjio")
    two   = getUser (UserId "dmj")
    three = getUser (UserId "abs")


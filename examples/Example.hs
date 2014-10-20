{-# LANGUAGE OverloadedStrings #-}
module Example where

import           Control.Monad  (liftM2)
import           Web.HackerNews (UserId (..), getUser, hackerNews)

main :: IO ()
main = print =<< hackerNews (liftM2 (,) one two)
  where
    one   = getUser (UserId "dmjio")
    two   = getUser (UserId "dmj")



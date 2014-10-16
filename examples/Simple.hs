module Simple where

import Web.HackerNews

-- | Simply get an item with default options and print it
main :: IO ()
main = print =<< getItem (ItemId 999)

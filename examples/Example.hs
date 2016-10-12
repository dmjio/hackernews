{-# LANGUAGE OverloadedStrings #-}
module Main where

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Web.HackerNews

main :: IO ()
main = do
 mgr <- newManager tlsManagerSettings
 print =<< getItem mgr (ItemId 1000)
 print =<< getUser mgr (UserId "dmjio")
 print =<< getMaxItem mgr
 print =<< getTopStories mgr
 print =<< getNewStories mgr
 print =<< getBestStories mgr
 print =<< getAskStories mgr
 print =<< getShowStories mgr
 print =<< getJobStories mgr
 print =<< getUpdates mgr

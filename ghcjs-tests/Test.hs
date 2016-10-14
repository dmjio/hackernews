{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE RecordWildCards      #-}
module Main where

import Control.Applicative
import Data.Aeson
import Data.Either               (isRight)
import Data.JSString
import System.Exit
import Test.Hspec (it, hspec, describe, shouldSatisfy, shouldBe)
import Test.Hspec.Core.Runner (hspecResult, Summary(..))
import Test.QuickCheck

import Web.HackerNews.Types
import Web.HackerNews

main :: IO ()
main = do
 Summary{..} <- hspecResult $ do
  describe "HackerNews API tests" $ do
    it "should round trip Updates JSON" $ property $ \(x :: Updates) ->
       Just x == decode (encode x)
    it "should round trip Item JSON" $ property $ \(x :: Item) ->
       Just x == decode (encode x)
    it "should round trip User JSON" $ property $ \(x :: User) ->
       Just x == decode (encode x)
    it "should retrieve item" $ do
      (`shouldSatisfy` isRight) =<< getItem (ItemId 1000)
    it "should return NotFound " $ do
      Left x <- getItem (ItemId 0)
      x `shouldBe` NotFound
    it "should retrieve user" $ do
      (`shouldSatisfy` isRight) =<< getUser (UserId "dmjio")
    it "should retrieve max item" $ do
      (`shouldSatisfy` isRight) =<< getMaxItem
    it "should retrieve top stories" $ do
      (`shouldSatisfy` isRight) =<< getTopStories
    it "should retrieve new stories" $ do
      (`shouldSatisfy` isRight) =<< getNewStories
    it "should retrieve best stories" $ do
      (`shouldSatisfy` isRight) =<< getBestStories
    it "should retrieve ask stories" $ do
      (`shouldSatisfy` isRight) =<< getAskStories
    it "should retrieve show stories" $ do
      (`shouldSatisfy` isRight) =<< getShowStories
    it "should retrieve job stories" $ do
      (`shouldSatisfy` isRight) =<< getJobStories
    it "should retrieve updates" $ do
      (`shouldSatisfy` isRight) =<< getUpdates
 case summaryFailures of
   x | x > 0 -> putStrLn "error"
     | otherwise -> putStrLn "done"




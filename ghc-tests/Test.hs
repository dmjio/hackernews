{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Data.Aeson
import Data.Either               (isRight)
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Test.Hspec                (it, hspec, describe, shouldSatisfy, shouldBe)
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Web.HackerNews

main :: IO ()
main = do
 mgr <- newManager tlsManagerSettings
 hspec $ do
  describe "HackerNews API tests" $ do
    it "should round trip Updates JSON" $ property $ \(x :: Updates) ->
       Just x == decode (encode x)
    it "should round trip Item JSON" $ property $ \(x :: Item) ->
       Just x == decode (encode x)
    it "should round trip User JSON" $ property $ \(x :: User) ->
       Just x == decode (encode x)
    it "should retrieve item" $ do
      (`shouldSatisfy` isRight) =<< getItem mgr (ItemId 1000)
    it "should return NotFound " $ do
      Left x <- getItem mgr (ItemId 0)
      x `shouldBe` NotFound
    it "should retrieve user" $ do
      (`shouldSatisfy` isRight) =<< getUser mgr (UserId "dmjio")
    it "should retrieve max item" $ do
      (`shouldSatisfy` isRight) =<< getMaxItem mgr
    it "should retrieve top stories" $ do
      (`shouldSatisfy` isRight) =<< getTopStories mgr
    it "should retrieve new stories" $ do
      (`shouldSatisfy` isRight) =<< getNewStories mgr
    it "should retrieve best stories" $ do
      (`shouldSatisfy` isRight) =<< getBestStories mgr
    it "should retrieve ask stories" $ do
      (`shouldSatisfy` isRight) =<< getAskStories mgr
    it "should retrieve show stories" $ do
      (`shouldSatisfy` isRight) =<< getShowStories mgr
    it "should retrieve job stories" $ do
      (`shouldSatisfy` isRight) =<< getJobStories mgr
    it "should retrieve updates" $ do
      (`shouldSatisfy` isRight) =<< getUpdates mgr











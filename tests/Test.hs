{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE ScopedTypeVariables  #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Main where

import Control.Applicative
import Data.Aeson
import Data.Either               (isRight)
import Generics.SOP.Arbitrary
import Generics.SOP.Universe
import Network.HTTP.Client
import Network.HTTP.Client.TLS
import Test.Hspec                (it, hspec, describe, shouldSatisfy, shouldBe)
import Test.QuickCheck
import Test.QuickCheck.Instances ()
import Web.HackerNews

instance Generic Item
instance Generic Updates
instance Generic User
instance Generic UserName
instance Generic UserId
instance Generic Delay
instance Generic Created
instance Generic Karma
instance Generic About
instance Generic Submitted

instance Arbitrary Item where arbitrary = garbitrary
instance Arbitrary User where arbitrary = garbitrary
instance Arbitrary Updates where arbitrary = garbitrary

instance Arbitrary UserName where arbitrary = garbitrary
instance Arbitrary UserId  where arbitrary = garbitrary
instance Arbitrary Delay where arbitrary = garbitrary
instance Arbitrary Created where arbitrary = garbitrary
instance Arbitrary Karma where arbitrary = garbitrary
instance Arbitrary About where arbitrary = garbitrary
instance Arbitrary Submitted where arbitrary = garbitrary

instance Arbitrary ItemId where arbitrary = garbitrary
instance Generic ItemId
instance Arbitrary Deleted where arbitrary = garbitrary
instance Generic Deleted
instance Arbitrary ItemType where arbitrary = garbitrary
instance Generic ItemType where
instance Arbitrary Time where arbitrary = garbitrary
instance Generic Time
instance Arbitrary ItemText where arbitrary = garbitrary
instance Generic ItemText
instance Arbitrary Dead where arbitrary = garbitrary
instance Generic Dead
instance Arbitrary Parent where arbitrary = garbitrary
instance Generic Parent
instance Arbitrary Kids where arbitrary = garbitrary
instance Generic Kids
instance Arbitrary URL where arbitrary = garbitrary
instance Generic URL
instance Arbitrary Score where arbitrary = garbitrary
instance Generic Score
instance Arbitrary Title where arbitrary = garbitrary
instance Generic Title
instance Arbitrary Parts where arbitrary = garbitrary
instance Generic Parts
instance Arbitrary Descendants where arbitrary = garbitrary
instance Generic Descendants

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











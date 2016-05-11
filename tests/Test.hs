{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Either    (isRight)
import           Test.Hspec     (it, hspec, describe, shouldSatisfy)
import           Web.HackerNews
import           Control.Applicative

main :: IO ()
main = hspec $ do
  describe "Hacker News API Tests" $ do
    it "Retrieves all" $ do
      result <- hackerNews $ (,,,,,,,,,) <$>
        getStory   (StoryId 83)          <*>  -- Story without kids
        getStory   (StoryId 8863)        <*>  -- Story with kids
        getComment (CommentId 2921983)   <*>
        getUser    (UserId "dmjio")      <*>
        getJob     (JobId 8437631)       <*>
        getPoll    (PollId 126809)       <*>
        getPollOpt (PollOptId 160705)    <*>
        getTopStories                    <*>
        getMaxItem                       <*>
        getUpdates
      result `shouldSatisfy` isRight

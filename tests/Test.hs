{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Maybe     (isJust)
import           Test.Hspec     (it, runIO, hspec, describe)
import           Web.HackerNews
import           Control.Applicative

main :: IO ()
main = hspec $ do
  describe "Hacker News API Tests" $ do
    (story, comment, user, poll, pollOpt, topStories, maxItem, updates) <- 
      runIO $ hackerNews $ (,,,,,,,)   <$> 
        getStory   (StoryId 8863)      <*>
        getComment (CommentId 2921983) <*>
        getUser    (UserId "dmjio")    <*>
        getPoll    (PollId 126809)     <*>
        getPollOpt (PollOptId 160705)  <*>
        getTopStories                  <*>
        getMaxItem                     <*>
        getUpdates
    it "Retrieves a Story"     $ isJust story
    it "Retrieves a Comment"   $ isJust comment
    it "Retrieves a User"      $ isJust user
    it "Retrieves a Poll"      $ isJust poll
    it "Retrieves a Pollopt"   $ isJust pollOpt
    it "Retrieves Top Stories" $ isJust topStories
    it "Retrieves Max Item"    $ isJust maxItem
    it "Retrieves Updates"     $ isJust updates











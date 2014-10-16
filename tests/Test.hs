{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Data.Maybe     (isJust)
import           Test.Hspec     (it, runIO, hspec, describe)
import           Web.HackerNews
import           Control.Applicative
import           Network.Wreq   (withManager)

main :: IO ()
main = withManager $ \opts -> hspec $ do
  describe "Hacker News API Tests" $ do 
    (story, comment, user, job, poll, pollOpt, topStories, maxItem, updates) <- 
      runIO $ (,,,,,,,,)                        <$> 
        getStoryWith opts   (StoryId 8863)      <*>
        getCommentWith opts (CommentId 2921983) <*>
        getUserWith opts    (UserId "dmjio")    <*>
        getJobWith opts     (JobId 8437631)     <*>
        getPollWith opts    (PollId 126809)     <*>
        getPollOptWith opts (PollOptId 160705)  <*>
        getTopStoriesWith opts                  <*>
        getMaxItemWith opts                     <*>
        getUpdatesWith opts
    it "Retrieves a Story"     $ isJust story
    it "Retrieves a Comment"   $ isJust comment
    it "Retrieves a User"      $ isJust user
    it "Retrieves a Job"       $ isJust job
    it "Retrieves a Poll"      $ isJust poll
    it "Retrieves a Pollopt"   $ isJust pollOpt
    it "Retrieves Top Stories" $ isJust topStories
    it "Retrieves Max Item"    $ isJust maxItem
    it "Retrieves Updates"     $ isJust updates











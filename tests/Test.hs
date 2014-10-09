module Main where

import           Data.Maybe     (isJust)
import           Test.Hspec     (it, runIO, hspec, describe)
import           Web.HackerNews

main :: IO ()
main = hspec $ do
  describe "Hacker News API Tests" $ do
    story <- runIO $ getStory (StoryId 8863)
    it "Retrieves a Story" $ isJust story
    comment <- runIO $ getComment (CommentId 2921983)
    it "Retrieves a Comment" $ isJust comment
    user <- runIO $ getUser (UserId "dmjio")
    it "Retrieves a User" $ isJust user
    poll <- runIO $ getPoll (PollId 126809)
    it "Retrieves a Poll" $ isJust poll
    pollOpt <- runIO $ getPollOpt (PollOptId 160705)
    it "Retrieves a Pollopt" $ isJust pollOpt
    topStories <- runIO getTopStories
    it "Retrieves Top Stories" $ isJust topStories
    maxItem <- runIO getMaxItem
    it "Retrieves Max Item" $ isJust maxItem
    updates <- runIO getUpdates
    it "Retrieves Updates" $ isJust updates











module Main where

import Test.Hspec
import Web.HackerNews
import Data.Maybe (isJust)

main :: IO ()
main = hspec $ do
  describe "Hacker News API Tests" $ do
    story <- runIO $ getStory (StoryId 8863)
    runIO $ print story
    it "Retrieves a Story" $ isJust story
         
  

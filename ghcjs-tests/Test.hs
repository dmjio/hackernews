module Main where

import Test.Hspec

main :: IO ()
main = hspec $ describe "HackerNews GHCJS tests" $ do
  it "Should do the right thing" $ do
    (4 + 4) `shouldBe` 8

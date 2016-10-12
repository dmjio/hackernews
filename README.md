hackernews
==========
![Hackage](https://img.shields.io/hackage/v/hackernews.svg)
![Hackage Dependencies](https://img.shields.io/hackage-deps/v/hackernews.svg)
![Haskell Programming Language](https://img.shields.io/badge/language-Haskell-blue.svg)
![MIT License](http://img.shields.io/badge/license-MIT-brightgreen.svg)

Hacker News API for Haskell

###Documentation
<https://github.com/HackerNews/API>

Now it supports GHCJS and can be used in the browser! Just install it using:
```bash
cabal install --ghcjs
```

###Tests
```bash
cabal configure && cabal test
```

```bash
HackerNews API tests
  should round trip Updates JSON
  should round trip Item JSON
  should round trip User JSON
  should retrieve item
  should retrieve user
  should retrieve max item
  should retrieve top stories
  should retrieve new stories
  should retrieve best stories
  should retrieve ask stories
  should retrieve show stories
  should retrieve job stories
  should retrieve updates

  Finished in 1.2129 seconds
  13 examples, 0 failures
```

###Usage
```haskell
module Main where

import Network.HTTP.Client
import Network.HTTP.Client.TLS

import Web.HackerNews

main :: IO ()
main = do
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
```

```bash
Right ( Item {
	 itemId = Just (ItemId 1000)
   , itemDeleted = Nothing
   , itemType = Story
   , itemBy = Just (UserName "python_kiss")
   , itemTime = Just (Time 1172394646)
   , itemText = Nothing
   , itemDead = Nothing
   , itemParent = Nothing
   , itemKids = Nothing
   , itemURL = Just (URL "http://www.netbusinessblog.com/2007/02/19/how-important-is-the-dot-com/")
   , itemScore = Just (Score 4)
   , itemTitle = Just (Title "How Important is the .com TLD?")
   , itemParts = Nothing
   , itemDescendants = Just (Descendants 0)
   })
Right (User {userId = UserId "dmjio"
		   , userDelay = Nothing
		   , userCreated = Created 1375807763
		   , userKarma = Karma 7
		   , userAbout = Nothing
		   , userSubmitted = Just (Submitted [11966297,9355613, ...])
		   })
Right (MaxItem 12695220)
Right (TopStories [12694004,12692190,12691597,...])
Right (NewStories [12695214,12695213,12695195,...])
Right (BestStories [12649414,12637126,12684980, ...])
Right (AskStories [12694706,12694401,12694038, ...])
Right (ShowStories [12694004,12692190,12695037, ...])
Right (JobStories [12693320,12691627,12690539,...])
Right (Updates { items = [12694916,12694478,12693674,..],
				 profiles = [UserName "stefano", UserName "chillydawg", ...]
			   })

```

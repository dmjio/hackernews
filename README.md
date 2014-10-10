hackernews [![Hackage](https://img.shields.io/hackage/v/hackernews.svg?style=flat)](https://hackage.haskell.org/package/hackernews)
==========
Hacker News API for Haskell

###Documentation
<https://github.com/HackerNews/API>

###Tests
```bash
cabal configure && cabal test
```

```bash
Hacker News API Tests
  - Retrieves a Story
  - Retrieves a Comment
  - Retrieves a User
  - Retrieves a Poll
  - Retrieves a Pollopt
  - Retrieves Top Stories
  - Retrieves Max Item
  - Retrieves Updates

Finished in 0.0019 seconds
8 examples, 0 failures
```

###Usage
```haskell 
module Example where

import           Control.Monad  (liftM3)
import           Web.HackerNews (UserId (..), getUser, hackerNews)

main :: IO ()
main = print =<< hackerNews (liftM3 (,,) one two three)
  where
    one   = getUser (UserId "dmjio")
    two   = getUser (UserId "dmj")
    three = getUser (UserId "abs")
```

```bash
(Just (User { userAbout = Just ""
            , userCreated = 2013-08-06 16:49:23 UTC
            , userDelay = 0
            , userId = UserId "dmjio"
            , userKarma = 6
            , userSubmitted = [8433827,8429256,8429161,8429069,8374809,8341570,7919268,7825469,7350544,7327291,6495994,6352317,6168527,6168524,6167639]})
      , 
Just (User { userAbout = Nothing
           , userCreated = 2007-04-11 05:57:35 UTC
           , userDelay = 0
           , userId = UserId "dmj"
           , userKarma = 1
           , userSubmitted = [11737]
           }),
Nothing)
```
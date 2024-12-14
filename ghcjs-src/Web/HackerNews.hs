{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings   #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.HackerNews
-- Copyright   : (c) David Johnson, 2014-2025
-- Maintainer  : code@dmj.io
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.HackerNews
       ( -- * API functions
         getItem
       , getUser
       , getMaxItem
       , getTopStories
       , getNewStories
       , getBestStories
       , getAskStories
       , getShowStories
       , getJobStories
       , getUpdates
       -- * Core Types
       , Item        (..)
       , User        (..)
       , Updates     (..)
       , MaxItem     (..)
       , TopStories  (..)
       , NewStories  (..)
       , BestStories (..)
       , AskStories  (..)
       , ShowStories (..)
       , JobStories  (..)
       --- * Supporting Types
       , UserId      (..)
       , ItemId      (..)
       , Deleted     (..)
       , ItemType    (..)
       , UserName    (..)
       , Time        (..)
       , ItemText    (..)
       , Dead        (..)
       , Parent      (..)
       , Kids        (..)
       , URL         (..)
       , Score       (..)
       , Title       (..)
       , Parts       (..)
       , Descendants (..)
       , Delay       (..)
       , Created     (..)
       , Karma       (..)
       , About       (..)
       , Submitted   (..)
       ) where

------------------------------------------------------------------------------
import Data.Aeson
import Data.Monoid
import JavaScript.Web.XMLHttpRequest
import Data.String.Conversions
import Data.JSString
import Data.JSString.Text

import Web.HackerNews.Types

-- | Retrieve `Item`
getItem :: ItemId -> IO (Either HackerNewsError Item)
getItem (ItemId x) = issueAjax (Just "item") $ textToJSString $ cs (show x)

-- | Retrieve `User`
getUser :: UserId -> IO (Either HackerNewsError User)
getUser (UserId u) = issueAjax (Just "user") (textToJSString u)

-- | Retrieve `MaxItem`
getMaxItem :: IO (Either HackerNewsError MaxItem)
getMaxItem = issueAjax Nothing "maxitem"

-- | Retrieve `TopStories`
getTopStories :: IO (Either HackerNewsError TopStories)
getTopStories = issueAjax Nothing "topstories"

-- | Retrieve `NewStories`
getNewStories :: IO (Either HackerNewsError NewStories)
getNewStories = issueAjax Nothing "newstories"

-- | Retrieve `BestStories`
getBestStories :: IO (Either HackerNewsError BestStories)
getBestStories = issueAjax Nothing "beststories"

-- | Retrieve `AskStories`
getAskStories :: IO (Either HackerNewsError AskStories)
getAskStories = issueAjax Nothing "askstories"

-- | Retrieve `ShowStories`
getShowStories :: IO (Either HackerNewsError ShowStories)
getShowStories = issueAjax Nothing "showstories"

-- | Retrieve `JobStories`
getJobStories :: IO (Either HackerNewsError JobStories)
getJobStories = issueAjax Nothing "jobstories"

-- | Retrieve `Updates`
getUpdates :: IO (Either HackerNewsError Updates)
getUpdates = issueAjax Nothing "updates"

issueAjax :: FromJSON a => Maybe JSString -> JSString -> IO (Either HackerNewsError a)
issueAjax maybePath uri = do
  response <- xhrByteString request
  pure $ case contents response of
    Nothing -> Left NotFound
    Just "null" -> Left NotFound
    Just x ->
      case eitherDecode (cs x) of
        Left l -> Left $ DecodeFailureError (cs l) mempty
        Right r -> Right r
    where
      request = Request GET url Nothing [] False NoData
      url = "https://hacker-news.firebaseio.com/v0/"
              <> maybe mempty (\path -> path <> "/") maybePath
              <> uri
              <> ".json"

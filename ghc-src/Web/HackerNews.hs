{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.HackerNews
-- Copyright   : (c) David Johnson, 2014-2016
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Haskell port of <https://github.com/HackerNews/API>
--
------------------------------------------------------------------------------
module Web.HackerNews
  ( -- * Hacker News API
    HackerNewsAPI
   -- * Custom combinators
  , HackerCapture
   -- * API functions
  , getItem
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
  --- * Error handling
  , HackerNewsError (..)
  ) where

import           Control.Monad.Trans.Except
import           Data.Bifunctor
import           Data.Monoid
import           Data.Proxy
import           Data.String.Conversions
import qualified Data.Text                  as T
import           Network.HTTP.Client        (Manager)
import           Network.HTTP.Types.Status
import           Servant.API
import           Servant.Client
import           Servant.Common.Req

import           Web.HackerNews.Types

-- | HackerNews API
type HackerNewsAPI =
       "item" :> HackerCapture ItemId :> Get '[JSON] Item
  :<|> "user" :> HackerCapture UserId :> Get '[JSON] User
  :<|> "maxitem.json" :> Get '[JSON] MaxItem
  :<|> "topstories.json" :> Get '[JSON] TopStories
  :<|> "newstories.json" :> Get '[JSON] NewStories
  :<|> "beststories.json" :> Get '[JSON] BestStories
  :<|> "askstories.json" :> Get '[JSON] AskStories
  :<|> "showstories.json" :> Get '[JSON] ShowStories
  :<|> "jobstories.json" :> Get '[JSON] JobStories
  :<|> "updates.json" :> Get '[JSON] Updates

-- | Custom combinator for appending '.json' to `Item` query
data HackerCapture (a :: *)

-- | Custom combinator `HasClient` instance
instance (ToHttpApiData a, HasClient api) => HasClient (HackerCapture a :> api) where
  type Client (HackerCapture a :> api) = a -> Client api
  clientWithRoute Proxy req val =
    clientWithRoute (Proxy :: Proxy api)
       (appendToPath p req)
    where
      p = T.unpack $ toUrlPiece val <> ".json"

-- | HN `BaseURL`
hackerNewsURL :: BaseUrl
hackerNewsURL = BaseUrl Https "hacker-news.firebaseio.com" 443 "/v0"

-- | Convert ServantError to HackerNewsError
toError :: Either ServantError ok -> Either HackerNewsError ok
toError = first go
  where
    go :: ServantError -> HackerNewsError
    go (FailureResponse Status{..} _ body) =
      FailureResponseError statusCode (cs statusMessage) (cs body)
    go (DecodeFailure _ _ "null") = NotFound
    go (DecodeFailure err _ body) =
      DecodeFailureError (cs err) (cs body)
    go (UnsupportedContentType _ body) =
      UnsupportedContentTypeError (cs body)
    go (InvalidContentTypeHeader header body) =
      InvalidContentTypeHeaderError (cs header) (cs body)
    go (ConnectionError ex) =
      HNConnectionError $ cs (show ex)

-- | Retrieve `Item`
getItem :: Manager -> ItemId -> IO (Either HackerNewsError Item)
getItem mgr itemId =
  toError <$> do
    runExceptT $ getItem' itemId mgr hackerNewsURL

-- | Retrieve `User`
getUser :: Manager -> UserId -> IO (Either HackerNewsError User)
getUser mgr userId =
  toError <$> do
    runExceptT $ getUser' userId mgr hackerNewsURL

-- | Retrieve `MaxItem`
getMaxItem :: Manager -> IO (Either HackerNewsError MaxItem)
getMaxItem mgr =
  toError <$> do
    runExceptT $ getMaxItem' mgr hackerNewsURL

-- | Retrieve `TopStories`
getTopStories :: Manager -> IO (Either HackerNewsError TopStories)
getTopStories mgr =
  toError <$> do
    runExceptT $ getTopStories' mgr hackerNewsURL

-- | Retrieve `NewStories`
getNewStories :: Manager -> IO (Either HackerNewsError NewStories)
getNewStories mgr =
  toError <$> do
    runExceptT $ getNewStories' mgr hackerNewsURL

-- | Retrieve `BestStories`
getBestStories :: Manager -> IO (Either HackerNewsError BestStories)
getBestStories mgr =
  toError <$> do
    runExceptT $ getBestStories' mgr hackerNewsURL

-- | Retrieve `AskStories`
getAskStories :: Manager -> IO (Either HackerNewsError AskStories)
getAskStories mgr =
  toError <$> do
    runExceptT $ getAskStories' mgr hackerNewsURL

-- | Retrieve `ShowStories`
getShowStories :: Manager -> IO (Either HackerNewsError ShowStories)
getShowStories mgr =
  toError <$> do
    runExceptT $ getShowStories' mgr hackerNewsURL

-- | Retrieve `JobStories`
getJobStories :: Manager -> IO (Either HackerNewsError JobStories)
getJobStories mgr =
  toError <$> do
    runExceptT $ getJobStories' mgr hackerNewsURL

-- | Retrieve `Updates`
getUpdates :: Manager -> IO (Either HackerNewsError Updates)
getUpdates mgr =
  toError <$> do
    runExceptT $ getUpdates' mgr hackerNewsURL

getItem' :: ItemId -> Manager -> BaseUrl -> ClientM Item
getUser' :: UserId -> Manager -> BaseUrl -> ClientM User
getMaxItem' :: Manager -> BaseUrl -> ClientM MaxItem
getTopStories' :: Manager -> BaseUrl -> ClientM TopStories
getNewStories' :: Manager -> BaseUrl -> ClientM NewStories
getBestStories' :: Manager -> BaseUrl -> ClientM BestStories
getAskStories' :: Manager -> BaseUrl -> ClientM AskStories
getShowStories' :: Manager -> BaseUrl -> ClientM ShowStories
getJobStories' :: Manager -> BaseUrl -> ClientM JobStories
getUpdates' :: Manager -> BaseUrl -> ClientM Updates

getItem'
  :<|> getUser'
  :<|> getMaxItem'
  :<|> getTopStories'
  :<|> getNewStories'
  :<|> getBestStories'
  :<|> getAskStories'
  :<|> getShowStories'
  :<|> getJobStories'
  :<|> getUpdates' = client (Proxy :: Proxy HackerNewsAPI)


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
import           Data.Aeson
import           Data.Aeson.Types
import           Data.Bifunctor
import           Data.Char
import           Data.Monoid
import           Data.Proxy
import           Data.String.Conversions
import qualified Data.Text                  as T
import           GHC.Generics
import           Network.HTTP.Client        (Manager)
import           Network.HTTP.Types.Status
import           Servant.API
import           Servant.Client
import           Servant.Common.Req

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

-- | Error handling for `HackerNewsAPI`
data HackerNewsError
  = NotFound
  | FailureResponseError Int T.Text T.Text
  | HNConnectionError T.Text
  | DecodeFailureError T.Text T.Text
  | InvalidContentTypeHeaderError T.Text T.Text
  | UnsupportedContentTypeError T.Text
  deriving (Show, Eq)

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

-- | The item and profile changes are at <https://hacker-news.firebaseio.com/v0/updates>
data Updates = Updates  {
      items :: [Int]
       -- ^ Updated `Item`s
    , profiles :: [UserName]
       -- ^ Updated `UserName`s
   } deriving (Show, Eq, Generic)

instance ToJSON Updates
instance FromJSON Updates

-- | The current largest item id is at <https://hacker-news.firebaseio.com/v0/maxitem>.
-- You can walk backward from here to discover all items.
newtype MaxItem = MaxItem Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | <https://hacker-news.firebaseio.com/v0/topstories>
newtype TopStories = TopStories [Int]
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | <https://hacker-news.firebaseio.com/v0/newstories>
newtype NewStories = NewStories [Int]
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | <https://hacker-news.firebaseio.com/v0/beststories>
newtype BestStories = BestStories [Int]
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | <https://hacker-news.firebaseio.com/v0/askstories>
newtype AskStories = AskStories [Int]
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | <https://hacker-news.firebaseio.com/v0/showstories>
newtype ShowStories = ShowStories [Int]
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | <https://hacker-news.firebaseio.com/v0/jobstories>
newtype JobStories = JobStories [Int]
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | Users are identified by case-sensitive ids, and live under
-- <https://hacker-news.firebaseio.com/v0/user/>.
-- Only users that have public activity (comments or story submissions)
-- on the site are available through the API.
data User = User {
    userId :: UserId
    -- ^ The user's unique username. Case-sensitive. Required.
  , userDelay :: Maybe Delay
    -- ^ Delay in minutes between a comment's creation and its visibility to other users.
  , userCreated :: Created
    -- ^ Creation date of the user, in Unix Time.
  , userKarma :: Karma
    -- ^ The user's karma
  , userAbout :: Maybe About
    -- ^ The user's optional self-description. HTML.
  , userSubmitted :: Maybe Submitted
    -- ^ List of the user's stories, polls and comments.
  } deriving (Show, Eq, Generic)

instance ToJSON User where
  toJSON = genericToJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 4
  }

instance FromJSON User where
  parseJSON = genericParseJSON defaultOptions {
    fieldLabelModifier = map toLower . drop 4
  }

-- | The user's karma.
newtype Karma = Karma Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | The user's unique username. Case-sensitive. Required.
newtype UserId = UserId T.Text
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData, Generic)

-- | Delay in minutes between a comment's creation and its visibility to other users.
newtype Delay = Delay Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | Creation date of the user, in Unix Time.
newtype Created = Created Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | The user's optional self-description. HTML.
newtype About = About T.Text
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | List of the user's stories, polls and comments.
newtype Submitted = Submitted [Int]
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | The item's unique id.
newtype ItemId = ItemId Int
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData, Generic)

-- | `true` if the item is deleted.
newtype Deleted = Deleted Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | The type of item. One of "job", "story", "comment", "poll", or "pollopt"
data ItemType = Job | Story | Comment | Poll | PollOpt
  deriving (Show, Eq, Generic)

instance ToJSON ItemType where
  toJSON = genericToJSON
    defaultOptions { constructorTagModifier = map toLower }

instance FromJSON ItemType where
  parseJSON = genericParseJSON
    defaultOptions { constructorTagModifier = map toLower }

-- | The username of the item's author.
newtype UserName = UserName T.Text
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | The comment, story or poll text. HTML.
newtype ItemText = ItemText T.Text
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | `true` if the item is dead.
newtype Dead = Dead Bool
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | The item's parent. For comments, either another comment or the relevant story.
-- For pollopts, the relevant poll.
newtype Parent = Parent Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | Creation date of the item, in Unix Time.
newtype Time = Time Integer
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | The ids of the item's comments, in ranked display order.
newtype Kids = Kids [Int]
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | The URL of the story.
newtype URL = URL T.Text
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | The story's score, or the votes for a pollopt.
newtype Score = Score Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | The title of the story, poll or job.
newtype Title = Title T.Text
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | A list of related pollopts, in display order.
newtype Parts = Parts [Int]
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | In the case of stories or polls, the total comment count.
newtype Descendants = Descendants Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic)

-- | Stories, comments, jobs, Ask HNs and even polls are just items.
-- They're identified by their ids, which are unique integers, and live under
-- <https://hacker-news.firebaseio.com/v0/item/>.
data Item = Item {
      itemId          :: Maybe ItemId
    , itemDeleted     :: Maybe Deleted
    , itemType        :: ItemType
    , itemBy          :: Maybe UserName
    , itemTime        :: Maybe Time
    , itemText        :: Maybe ItemText
    , itemDead        :: Maybe Dead
    , itemParent      :: Maybe Parent
    , itemKids        :: Maybe Kids
    , itemURL         :: Maybe URL
    , itemScore       :: Maybe Score
    , itemTitle       :: Maybe Title
    , itemParts       :: Maybe Parts
    , itemDescendants :: Maybe Descendants
    } deriving (Show, Eq, Generic)

instance ToJSON Item where
  toJSON = genericToJSON
    defaultOptions { fieldLabelModifier = map toLower . drop 4 }

instance FromJSON Item where
  parseJSON = genericParseJSON
    defaultOptions { fieldLabelModifier = map toLower . drop 4 }

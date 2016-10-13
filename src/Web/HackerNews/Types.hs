{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.HackerNews.Types
-- Copyright   : (c) David Johnson, 2014-2016
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
-- Haskell port of <https://github.com/HackerNews/API>
--
------------------------------------------------------------------------------
module Web.HackerNews.Types where

import           Data.Aeson
import           Data.Aeson.Types
import           Data.Char
import qualified Data.Text        as T
import           GHC.Generics
import           Servant.API

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

-- | Error handling for `HackerNewsAPI`
data HackerNewsError
  = NotFound
  | FailureResponseError Int T.Text T.Text
  | HNConnectionError T.Text
  | DecodeFailureError T.Text T.Text
  | InvalidContentTypeHeaderError T.Text T.Text
  | UnsupportedContentTypeError T.Text
  deriving (Show, Eq)

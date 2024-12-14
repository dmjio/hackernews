{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.HackerNews.Types
-- Copyright   : (c) David Johnson, 2014-2025
-- Maintainer  : code@dmj.io
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

import Test.QuickCheck
import Test.QuickCheck.Instances ()

-- | The item and profile changes are at <https://hacker-news.firebaseio.com/v0/updates>
data Updates = Updates  {
      items :: [ItemId]
       -- ^ Updated `Item`s
    , profiles :: [UserId]
       -- ^ Updated `UserName`s
   } deriving (Show, Eq, Generic)

instance ToJSON Updates
instance FromJSON Updates

instance Arbitrary Updates where
  arbitrary = Updates <$> arbitrary <*> arbitrary

-- | The current largest item id is at <https://hacker-news.firebaseio.com/v0/maxitem>.
-- You can walk backward from here to discover all items.
newtype MaxItem = MaxItem ItemId
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | <https://hacker-news.firebaseio.com/v0/topstories>
newtype TopStories = TopStories [ItemId]
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | <https://hacker-news.firebaseio.com/v0/newstories>
newtype NewStories = NewStories [ItemId]
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | <https://hacker-news.firebaseio.com/v0/beststories>
newtype BestStories = BestStories [ItemId]
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | <https://hacker-news.firebaseio.com/v0/askstories>
newtype AskStories = AskStories [ItemId]
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | <https://hacker-news.firebaseio.com/v0/showstories>
newtype ShowStories = ShowStories [ItemId]
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | <https://hacker-news.firebaseio.com/v0/jobstories>
newtype JobStories = JobStories [ItemId]
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

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

instance Arbitrary User where
  arbitrary =
    User <$> arbitrary <*> arbitrary <*> arbitrary
         <*> arbitrary <*> arbitrary <*> arbitrary

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
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | The user's unique username. Case-sensitive. Required.
newtype UserId = UserId T.Text
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData, Generic, Arbitrary)

-- | Delay in minutes between a comment's creation and its visibility to other users.
newtype Delay = Delay Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | Creation date of the user, in Unix Time.
newtype Created = Created Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | The user's optional self-description. HTML.
newtype About = About T.Text
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | List of the user's stories, polls and comments.
newtype Submitted = Submitted [ItemId]
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | The item's unique id.
newtype ItemId = ItemId Int
  deriving (Show, Eq, ToJSON, FromJSON, ToHttpApiData, Generic, Arbitrary)

-- | `true` if the item is deleted.
newtype Deleted = Deleted Bool
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | The type of item. One of "job", "story", "comment", "poll", or "pollopt"
data ItemType = Job | Story | Comment | Poll | PollOpt
  deriving (Show, Eq, Generic, Enum)

instance Arbitrary ItemType where
  arbitrary = elements [ Job .. ]

instance ToJSON ItemType where
  toJSON = genericToJSON
    defaultOptions { constructorTagModifier = map toLower }

instance FromJSON ItemType where
  parseJSON = genericParseJSON
    defaultOptions { constructorTagModifier = map toLower }

-- | The comment, story or poll text. HTML.
newtype ItemText = ItemText T.Text
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | `true` if the item is dead.
newtype Dead = Dead Bool
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | The item's parent. For comments, either another comment or the relevant story.
-- For pollopts, the relevant poll.
newtype Parent = Parent ItemId
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | Creation date of the item, in Unix Time.
newtype Time = Time Integer
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | The ids of the item's comments, in ranked display order.
newtype Kids = Kids [ItemId]
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | The URL of the story.
newtype URL = URL T.Text
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | The story's score, or the votes for a pollopt.
newtype Score = Score Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | The title of the story, poll or job.
newtype Title = Title T.Text
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | A list of related pollopts, in display order.
newtype Parts = Parts [ItemId]
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | In the case of stories or polls, the total comment count.
newtype Descendants = Descendants Int
  deriving (Show, Eq, ToJSON, FromJSON, Generic, Arbitrary)

-- | Stories, comments, jobs, Ask HNs and even polls are just items.
-- They're identified by their ids, which are unique integers, and live under
-- <https://hacker-news.firebaseio.com/v0/item/>.
data Item = Item {
      itemId          :: Maybe ItemId
    , itemDeleted     :: Maybe Deleted
    , itemType        :: ItemType
    , itemBy          :: Maybe UserId
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

instance Arbitrary Item where
  arbitrary = Item <$> arbitrary <*> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary <*> arbitrary
                   <*> arbitrary <*> arbitrary

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

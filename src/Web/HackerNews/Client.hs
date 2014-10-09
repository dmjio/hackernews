{-# LANGUAGE OverloadedStrings #-}
module Web.HackerNews.Client ( getItem ) where

import           Data.Aeson                 hiding (Result)
import           Data.Aeson.Parser          (value)
import           Data.Attoparsec.ByteString (parseOnly)
import           Data.Either                (rights)

import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as T
import           Network.Http.Client
import           OpenSSL                    (withOpenSSL)
import qualified System.IO.Streams          as Streams

------------------------------------------------------------------------------
-- | HackerNews API request method
getItem
  :: FromJSON a
  => Text      
  -> IO (Maybe a)
getItem url = withOpenSSL $ do
  ctx <- baselineContextSSL
  con <- openConnectionSSL ctx "hacker-news.firebaseio.com" 443
  req <- buildRequest $ do
    http GET $ "/v0/" <> T.encodeUtf8 url <> ".json"
    setAccept "application/json"
  sendRequest con req emptyBody
  bytes <- receiveResponse con $ const $ Streams.read
  closeConnection con
  return $ case bytes of
   Nothing -> Nothing
   Just bs -> do
     let xs = rights [parseOnly value bs, parseOnly json bs]
     case xs of
       []    -> Nothing
       x : _ ->
         case fromJSON x of
          Success a -> Just a
          _         -> Nothing


{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
module Web.HackerNews.Client
       ( hackerNews
       , buildHNRequest
       , HackerNews
       ) where

import           Data.Aeson                 hiding (Result)
import           Data.Aeson.Parser          (value)
import           Data.Attoparsec.ByteString (parseOnly)
import           Data.Either                (rights)

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Monoid                ((<>))
import           Data.Text                  (Text)
import qualified Data.Text.Encoding         as T
import           Network.Http.Client
import           OpenSSL                    (withOpenSSL)
import qualified System.IO.Streams          as Streams

------------------------------------------------------------------------------
-- | Core Type
type HackerNews a = ReaderT Connection IO a

------------------------------------------------------------------------------
-- | HackerNews API request method
hackerNews :: (Show a, FromJSON a) => HackerNews a -> IO a
hackerNews requests =
  withOpenSSL $ do
    ctx <- baselineContextSSL
    con <- openConnectionSSL ctx "hacker-news.firebaseio.com" 443
    result <- flip runReaderT con requests
    closeConnection con
    return result

------------------------------------------------------------------------------
-- | Request Builder for API
buildHNRequest :: FromJSON a => Text -> HackerNews (Maybe a)
buildHNRequest url = do
    con <- ask
    liftIO $ do
      req <- buildRequest $ do
        http GET $ "/v0/" <> T.encodeUtf8 url <> ".json"
        setHeader "Connection" "Keep-Alive"
        setAccept "application/json"
      sendRequest con req emptyBody
      !bytes <- receiveResponse con $ const $ Streams.read
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


{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.HackerNews.Client
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.HackerNews.Client
       ( hackerNews
       , buildHNRequest
       , HackerNews
       , HackerNewsError (..)
       ) where

import           Data.Aeson                 hiding (Result)
import           Data.Aeson.Parser          (value)
import           Data.Attoparsec.ByteString (parseOnly)
import           Data.Either                (rights)
import           Control.Monad.Trans.Either  
import           Control.Exception          (try, SomeException)

import           Control.Monad.IO.Class     (liftIO)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Data.Monoid                ((<>))
import qualified Data.Text.Encoding         as T
import           Data.Text                 (Text)
import           Network.Http.Client
import           OpenSSL                    (withOpenSSL)
import qualified System.IO.Streams          as Streams

------------------------------------------------------------------------------
-- | Core Type
type HackerNews a = EitherT HackerNewsError (ReaderT Connection IO) a

------------------------------------------------------------------------------
-- | Error Types
data HackerNewsError =
    ConnectionError
  | ParseError
  | RequestError
  deriving (Show, Eq)

------------------------------------------------------------------------------
-- | HackerNews API request method
hackerNews :: FromJSON a => HackerNews a -> IO (Either HackerNewsError a)
hackerNews requests =
  withOpenSSL $ do
    ctx <- baselineContextSSL
    con <- try (openConnectionSSL ctx "hacker-news.firebaseio.com" 443) :: IO (Either SomeException Connection)
    case con of
     Left _ -> return $ Left ConnectionError
     Right conn -> do
       result <- flip runReaderT conn $ runEitherT requests
       closeConnection conn
       return result

------------------------------------------------------------------------------
-- | Request Builder for API
buildHNRequest :: FromJSON a => Text -> HackerNews a
buildHNRequest url = do
    con <- lift ask
    bytes <- liftIO $ do
      req <- buildRequest $ do
        http GET $ "/v0/" <> T.encodeUtf8 url <> ".json"
        setHeader "Connection" "Keep-Alive"
        setAccept "application/json"
      sendRequest con req emptyBody
      receiveResponse con $ const Streams.read
    case bytes of
      Nothing -> left RequestError
      Just bs -> do
        let xs = rights [parseOnly value bs, parseOnly json bs]
        case xs of
          []    -> left ParseError
          x : _ ->
            case fromJSON x of
             Success jsonBody -> right jsonBody
             _                -> left ParseError
    



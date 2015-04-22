{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface, JavaScriptFFI, DeriveGeneric, DeriveDataTypeable #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.HackerNews.Client
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
-- | 
------------------------------------------------------------------------------
module Web.HackerNews.Client
       ( hackerNews
       , buildHNRequest
       , HackerNews
       , HackerNewsError (..)
       ) where

------------------------------------------------------------------------------
import           Data.Aeson                 hiding (Result)
import           Data.Aeson.Parser          (value)
import qualified Data.Text.Encoding         as T
import           Data.Text                  (Text)
import           Data.Monoid                ((<>))
import           Control.Monad.Trans.Either
import           Data.Either                (rights)
import           Data.Maybe
import           Control.Monad.IO.Class     (liftIO)
import           Data.Attoparsec.ByteString (parseOnly)
import           Control.Monad              (when)
#ifdef __GHCJS__
import           GHCJS.Types
import           GHCJS.Foreign as F
#else
import           Control.Exception          (try, SomeException)
import           Control.Monad.Trans.Class  (lift)
import           Control.Monad.Trans.Reader (ReaderT, ask, runReaderT)
import           Network.Http.Client
import           OpenSSL                    (withOpenSSL)
import qualified System.IO.Streams          as Streams
#endif

------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Debug flag
debug :: Bool
debug = False

------------------------------------------------------------------------------
-- | Core Type
#ifdef __GHCJS__
type HackerNews a = EitherT HackerNewsError IO a
#else
type HackerNews a = EitherT HackerNewsError (ReaderT Connection IO) a
#endif
------------------------------------------------------------------------------
-- | Error Types
data HackerNewsError =
    ConnectionError
  | ParseError
  | NotFound
  | RequestError
  deriving (Show, Eq)

#ifdef __GHCJS__
-- | HackerNews API request method
hackerNews :: FromJSON a => HackerNews a -> IO (Either HackerNewsError a)
hackerNews = runEitherT


------------------------------------------------------------------------------
-- | Request Builder for API
buildHNRequest :: FromJSON a => Text -> HackerNews a
buildHNRequest path = do
  let url = "https://hacker-news.firebaseio.com/v0/" <> path <> ".json"
  res <- liftIO $ ajax url
  case (arError res) of
   Just et -> case et of
     "connection-error" -> left ConnectionError
     "request-error" -> left RequestError
     _ -> left NotFound
   Nothing -> do
     let t = T.encodeUtf8 $ fromMaybe "" $ arData res
         xs = rights [parseOnly value t, parseOnly json t]
     when debug $ liftIO . print $ t
     case xs of
      [] -> left ParseError
      x : _ ->
        case fromJSON x of
         Success jsonBody -> right jsonBody
         _                -> left NotFound


data AjaxResult = AjaxResult { arData :: Maybe Text,
                               arError :: Maybe Text
                             } deriving (Ord, Eq, Show)

ajax :: Text -> IO AjaxResult
ajax url = do
  res <- js_ajax (toJSString url)
  err <- F.getProp ("error" :: Text) res
  dat <- F.getProp ("data" :: Text) res
  let d = getTextDat dat
      e = getTextDat err
  return (AjaxResult d e)
  where getTextDat dt = if isNull dt then Nothing else Just (fromJSString dt)


foreign import javascript interruptible "var req = new XMLHttpRequest(); \
  if (!req)\
    $c({error: 'connection-error', data: null});\
  req.onreadystatechange = function() {\
    if (req.readyState === 4) {\
      if (req.status === 200) {\
        $c({data: req.responseText, error: null});\
      } else\
        $c({error: 'request-error', data: null});\
    }\
  };\
  req.open('GET', $1, true);\
  req.send();"
  js_ajax :: JSString -> IO (JSRef ajaxResult)

#else
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
        when debug $ liftIO . print $ bs
        let xs = rights [parseOnly value bs, parseOnly json bs]
        case xs of
          []    -> left ParseError
          x : _ ->
            case fromJSON x of
             Success jsonBody -> right jsonBody
             _                -> left NotFound
    
#endif

{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface, JavaScriptFFI #-}
------------------------------------------------------------------------------
-- |
-- Module      : Web.HackerNews
-- Copyright   : (c) David Johnson, 2014-2016
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
--
------------------------------------------------------------------------------
module Web.HackerNews
       ( hackerNews
       , buildHNRequest
       , HackerNews
       , HackerNewsError (..)
       ) where

------------------------------------------------------------------------------
import           Data.Aeson                 hiding (Result, Object)
import           Data.Aeson.Parser          (value)
import qualified Data.Text.Encoding         as T
import           Data.Text                  (Text)
import           Data.Monoid                ((<>))
import           Control.Monad.Trans.Except
import           Data.Either                (rights)
import           Data.Maybe
import           Control.Monad.IO.Class     (liftIO)
import           Data.Attoparsec.ByteString (parseOnly)
import           Control.Monad              (when)
import           GHCJS.Types
import           GHCJS.Marshal.Pure
import           Data.JSString.Text
import           JavaScript.Object

------------------------------------------------------------------------------
-- | Debug flag
debug :: Bool
debug = False

------------------------------------------------------------------------------
-- | Core Type
type HackerNews a = ExceptT HackerNewsError IO a
------------------------------------------------------------------------------
-- | Error Types
data HackerNewsError =
    ConnectionError
  | ParseError
  | NotFound
  | RequestError
  deriving (Show, Eq)

-- | HackerNews API request method
hackerNews :: FromJSON a => HackerNews a -> IO (Either HackerNewsError a)
hackerNews = runExceptT

------------------------------------------------------------------------------
-- | Request Builder for API
buildHNRequest :: FromJSON a => Text -> HackerNews a
buildHNRequest path = do
  let url = "https://hacker-news.firebaseio.com/v0/" <> path <> ".json"
  res <- liftIO $ ajax url
  case arError res of
   Just et -> case et of
     "connection-error" -> throwE ConnectionError
     "request-error" -> throwE RequestError
     _ -> throwE NotFound
   Nothing -> do
     let t = T.encodeUtf8 $ fromMaybe "" $ arData res
         xs = rights [parseOnly value t, parseOnly json t]
     when debug $ liftIO . print $ t
     case xs of
      [] -> throwE ParseError
      x : _ ->
        case fromJSON x of
         Success jsonBody -> pure jsonBody
         _                -> throwE NotFound

data AjaxResult = AjaxResult { arData :: Maybe Text,
                               arError :: Maybe Text
                             } deriving (Ord, Eq, Show)

ajax :: Text -> IO AjaxResult
ajax url = do
  res <- js_ajax (textToJSString url)
  err <- getProp ("error" :: JSString) res
  dat <- getProp ("data" :: JSString) res
  let d = getTextDat dat
      e = getTextDat err
  return (AjaxResult d e)
  where getTextDat dt = if isNull dt then Nothing else Just (pFromJSVal dt)

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
  js_ajax :: JSString -> IO Object

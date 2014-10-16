{-# LANGUAGE OverloadedStrings #-}
-- |
-- Module      : Web.HackerNews.Client
-- Copyright   : (c) David Johnson, 2014
-- Maintainer  : djohnson.m@gmail.com
-- Stability   : experimental
-- Portability : POSIX
module Web.HackerNews.Client (makeRequestWith) where

import Control.Lens
import Data.Aeson
import Data.Aeson.Lens
import Data.Text (Text, unpack)
import Network.Wreq

makeRequestWith :: FromJSON a => Options -> Text -> IO (Maybe a)
makeRequestWith opts path = do
    let opts' = opts & header "Accept" .~ ["application/json"]
    let url = "https://hacker-news.firebaseio.com/v0/" ++ unpack path ++ ".json"
    resp <- getWith opts' url
    return $ do
        val <- resp ^? responseBody . _Value . to fromJSON
        case val of
            Error _ -> Nothing
            (Success a) -> Just a

{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses , FunctionalDependencies #-}
module Web.HackerNews.Endpoint where

import           Data.Aeson          (FromJSON)
import           Data.Text           (Text, append)

import           Web.HackerNews.Client (HackerNews, buildHNRequest)
import           Web.HackerNews.Util   (toText)

-- | Endpoint maps the id to the returned type on a type level
-- | The function dependency @id -> resp@ specifies that @id@ uniquely determines @resp@
class Endpoint id resp | id -> resp where
    endpoint :: id -> Text -- | Turn @id@ into path that points to resource


itemEndpoint :: Int -> Text
itemEndpoint = append "item/" . toText

-- | Generic function for making requests
getEndpoint :: (Endpoint a b, FromJSON b) => a -> HackerNews (Maybe b)
getEndpoint id' = buildHNRequest $ endpoint id'

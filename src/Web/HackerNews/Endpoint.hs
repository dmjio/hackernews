{-# LANGUAGE OverloadedStrings, MultiParamTypeClasses , FunctionalDependencies #-}
module Web.HackerNews.Endpoint where

import           Data.Aeson            (FromJSON)
import           Data.Text             (Text, append)
import           Network.Wreq          (Options, defaults)

import           Web.HackerNews.Client (makeRequestWith)
import           Web.HackerNews.Util   (toText)

-- | Endpoint maps the id to the returned type on a type level
-- | The function dependency @id -> resp@ specifies that @id@ uniquely determines @resp@
class Endpoint id resp | id -> resp where
    endpoint :: id -> Text -- | Turn @id@ into path that points to resource


itemEndpoint :: Int -> Text
itemEndpoint = append "item/" . toText

-- | Generic function for making requests
getEndpointWith :: (Endpoint a b, FromJSON b) => Options -> a -> IO (Maybe b)
getEndpointWith opts = makeRequestWith opts . endpoint

-- | Generic function for making requests with default options
getEndpoint :: (Endpoint a b, FromJSON b) => a -> IO (Maybe b)
getEndpoint = makeRequestWith defaults . endpoint

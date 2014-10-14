{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}
module Web.HackerNews.Endpoint where

import           Data.Text (Text)

-- | Endpoint maps the id to the returned type on a type level
-- | The function dependency @id -> resp@ specifies that @id@ uniquely determines @resp@
class Endpoint id resp | id -> resp where
    endpoint :: id -> Text -- | Turn @id@ into path that points to resource

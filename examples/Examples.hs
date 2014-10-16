-- | You should have async, wreq, timeit and hackernews in order to compile this
module Examples where

import Control.Concurrent.Async (mapConcurrently)
import Control.Monad (forM_)
import System.TimeIt (timeIt)

import Web.HackerNews

import Network.Wreq (withManager)

ensureJust :: Maybe a -> IO ()
ensureJust Nothing = error "Request Failed."
ensureJust (Just _) = return ()

-- | Each example gets a number of items and verifies that all of them are @Just Something@
main :: IO ()
main = do
    -- | Ids of items that we gonna fetch
    let itemIds = map ItemId [1..10]
    
    ---------------------------------------------------------------------------
    
    -- | Here we open a new connection for each request
    putStrLn "[[[ Fetching 10 Items consequently ]]]"
    timeIt $ forM_ itemIds $ \id' -> do
    	putStr $ "Fetching: " ++ show id'
    	getItem id'
    	putStrLn "\t\t[DONE]"
    
    ---------------------------------------------------------------------------
    
    -- | Opening a new connection is an expensive opperation, fortunately we can
    -- | share it. withManager gives us `Options` which has a manager that manages
    -- | our connections. Manager would open a new connection if it's necessary
    -- | and it would use an existing one in case if it's possible. In result
    -- | CPU time is reduced greatly. The real time is reduced slightly.
    putStrLn "[[[ Fetching 10 Items consequently with shared manager ]]]"
    timeIt $ withManager $ \opts -> forM_ itemIds $ \id' -> do
    	putStr $ "Fetching: " ++ show id'
    	getItemWith opts id'
    	putStrLn "\t\t[DONE]"
    
    ---------------------------------------------------------------------------
    
    -- | We also can share one manager across multiple threads. Making requests
    -- | concurrently would greatly reduce real time.
    putStrLn "[[[ Fetching 10 Items concurrently with shared manager ]]]"
    timeIt $ withManager $ \opts -> mapConcurrently (getItemWith opts) itemIds >> return ()
    
    ---------------------------------------------------------------------------
    
    -- | I really enjoy watching how it works concurrently. How it loads my 
    -- | network. So, one more time. 100 concurent requests.
    let itemIds = map ItemId [1..100]
    putStrLn "[[[ Fetching 100 Items concurrently with shared manager ]]]"
    timeIt $ withManager $ \opts -> mapConcurrently (getItemWith opts) itemIds >> return ()
	

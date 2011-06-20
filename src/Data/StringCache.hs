module Data.StringCache
       ( StringCache
       , empty
       , storeString
       , getStringMaybe
       , getString
       , hasString
       , wasteId
       ) where

import Control.Monad.State
import Data.HashMap (Map)
import qualified Data.HashMap as Map
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Text (Text)

data StringCache = 
  StringCache { strings :: IntMap Text
              , ids :: Map Text Int
              , stringCounter :: Int
              } deriving (Show)

empty :: StringCache
empty = StringCache IntMap.empty Map.empty 0

getStrings :: Monad m => StateT StringCache m (IntMap Text)
getStrings = return . strings =<< get

putStrings :: Monad m => IntMap Text -> StateT StringCache m ()
putStrings s = modify $ \ c -> c { strings = s }

getIds :: Monad m => StateT StringCache m (Map Text Int)
getIds = return . ids =<< get

putIds :: Monad m => Map Text Int -> StateT StringCache m ()
putIds i = modify $ \ c -> c { ids = i }

getCounter :: Monad m => StateT StringCache m Int
getCounter = return . stringCounter =<< get

putCounter :: Monad m => Int -> StateT StringCache m ()
putCounter counter = modify $ \ c -> c { stringCounter = counter }

incrementCounter :: Monad m => StateT StringCache m Int
incrementCounter = do
  counter <- getCounter
  let newCounter = counter + 1
  putCounter newCounter
  return newCounter

storeString :: Monad m => Text -> StateT StringCache m Int
storeString string = do
  idMap <- getIds
  if string `Map.member` idMap
    then return $ idMap Map.! string
    else do count <- incrementCounter
            stringMap <- getStrings
            putStrings $ IntMap.insert count string stringMap
            putIds $ Map.insert string count idMap
            return count

getStringMaybe :: Monad m => Int -> StateT StringCache m (Maybe Text)
getStringMaybe k = return . IntMap.lookup k =<< getStrings

getString :: Monad m => Int -> StateT StringCache m Text
getString k = return . (IntMap.! k) =<< getStrings

hasString :: Monad m => Int -> StateT StringCache m Bool
hasString k = return . IntMap.member k =<< getStrings

wasteId :: Monad m => StateT StringCache m Int
wasteId = incrementCounter
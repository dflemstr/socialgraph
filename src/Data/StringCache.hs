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
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
import Data.Text (Text)
import qualified Data.Text as Text

data StringCache =
  StringCache { strings :: IntMap Text
              , ids :: HashMap Text Int
              , stringCounter :: Int
              } deriving (Show)

empty :: StringCache
empty = StringCache IntMap.empty HashMap.empty 0

getStrings :: Monad m => StateT StringCache m (IntMap Text)
getStrings = return . strings =<< get

putStrings :: Monad m => IntMap Text -> StateT StringCache m ()
putStrings s = modify $ \ c -> c { strings = s }

getIds :: Monad m => StateT StringCache m (HashMap Text Int)
getIds = return . ids =<< get

putIds :: Monad m => HashMap Text Int -> StateT StringCache m ()
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
storeString str = do
  idMap <- getIds
  let string = Text.strip str
  case string `HashMap.lookup` idMap of
    Just key -> return key
    Nothing -> do
      count <- incrementCounter
      stringMap <- getStrings
      putStrings $ IntMap.insert count string stringMap
      putIds $ HashMap.insert string count idMap
      return count

getStringMaybe :: Monad m => Int -> StateT StringCache m (Maybe Text)
getStringMaybe k = return . IntMap.lookup k =<< getStrings

getString :: Monad m => Int -> StateT StringCache m Text
getString k = return . (IntMap.! k) =<< getStrings

hasString :: Monad m => Int -> StateT StringCache m Bool
hasString k = return . IntMap.member k =<< getStrings

wasteId :: Monad m => StateT StringCache m Int
wasteId = incrementCounter
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
import Data.Bimap (Bimap)
import qualified Data.Bimap as Bimap
import Data.Text (Text)

data StringCache = 
  StringCache { stringLookup :: Bimap Int Text
              , stringCounter :: Int
              } deriving (Show)

empty :: StringCache
empty = StringCache Bimap.empty 0

getLookup :: Monad m => StateT StringCache m (Bimap Int Text)
getLookup = return . stringLookup =<< get

putLookup :: Monad m => Bimap Int Text -> StateT StringCache m ()
putLookup l = modify $ \ c -> c { stringLookup = l }

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
  m <- getLookup
  if Bimap.memberR string m
    then return $ (Bimap.!>) m string
    else do c <- incrementCounter
            putLookup $ Bimap.insert c string m
            return c

getStringMaybe :: Monad m => Int -> StateT StringCache m (Maybe Text)
getStringMaybe k = return . Bimap.lookup k =<< getLookup

getString :: Monad m => Int -> StateT StringCache m Text
getString k = return . (Bimap.! k) =<< getLookup

hasString :: Monad m => Int -> StateT StringCache m Bool
hasString k = return . Bimap.member k =<< getLookup

wasteId :: Monad m => StateT StringCache m Int
wasteId = incrementCounter
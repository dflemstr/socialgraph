module Data.StringIntern (internString, internSymbol) where

import System.IO.Unsafe (unsafePerformIO)
import Data.Text (Text)
import qualified Data.Text as Text
import Data.HashTable (HashTable)
import qualified Data.HashTable as HashTable
import Data.Hashable (hash)

stringPool :: HashTable Text Text
stringPool = unsafePerformIO $ HashTable.new (==) $ fromIntegral . hash

internString :: Text -> Text
internString s = unsafePerformIO $ do
  intern <- HashTable.lookup stringPool s
  case intern of
    Just i  -> return i
    Nothing -> do
      HashTable.insert stringPool s s
      return s

internSymbol :: Text -> Text
internSymbol = internString . Text.toCaseFold
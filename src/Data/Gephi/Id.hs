{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Id where

import Data.Text (Text)
import qualified Data.Text as Text
import Data.Hashable

class (Ord a, Eq a, Hashable a) => Id a where
  gephiIdType :: a -> Text
  xmlId :: a -> String

instance Id Int where
  gephiIdType _ = "integer"
  xmlId = show

instance Id Text where
  gephiIdType _ = "string"
  xmlId = Text.unpack
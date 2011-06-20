{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Id where

import Text.XML.HXT.Arrow.Pickle
import Data.Text (Text)
import Data.Gephi.Util
import Data.Hashable

class (Ord a, Eq a, Hashable a) => Id a where
  gephiIdType :: a -> Text
  xpId :: PU a

instance Id Int where
  gephiIdType _ = "integer"
  xpId = xpInt

instance Id Text where
  gephiIdType _ = "string"
  xpId = xpTText
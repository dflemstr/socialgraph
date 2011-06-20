{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Meta where

import Text.XML.Light
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Gephi.Util
import qualified Data.Maybe as Maybe

data Meta =
  Meta { metaLastModifiedDate :: Maybe Text
       , metaCreator :: Maybe Text
       , metaKeywords :: Maybe Text
       , metaDescription :: Maybe Text
       }
  deriving (Show, Eq)

xmlMeta :: Meta -> Element
xmlMeta meta =
  Element (unqualified "meta")
  (maybe [] ((:[]) . attribute "lastmodifieddate" . Text.unpack) (metaLastModifiedDate meta))
  (Maybe.catMaybes [
    fmap (Elem . element "creator" . Text.unpack) $ metaCreator meta,
    fmap (Elem . element "keywords" . Text.unpack) $ metaKeywords meta,
    fmap (Elem . element "description" . Text.unpack) $ metaDescription meta]) Nothing

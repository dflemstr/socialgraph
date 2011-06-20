{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.AttributeDecl where

import Text.XML.Light
import qualified Data.Maybe as Maybe
import Data.Gephi.Mode
import Data.Gephi.Attribute
import Data.Gephi.Id
import Data.Gephi.Util

data AttributeDecl a =
  AttributeDecl { declClass :: Class
                , declMode :: Maybe Mode
                , declAttrs :: [Attribute a]
                }
  deriving (Show, Eq, Ord)

xmlAttributeDecl :: Id a => AttributeDecl a -> Element
xmlAttributeDecl decl =
  Element
  (unqualified "attributes")
  (Maybe.catMaybes [
      Just . attribute "class" . xmlClass . declClass $ decl,
      fmap (attribute "mode" . xmlMode) $ declMode decl])
  (map Elem . xmlAttributes . declAttrs $ decl) Nothing

xmlAttributes :: Id a => [Attribute a] -> [Element]
xmlAttributes = map xmlAttribute

data Class =
  ClassNode |
  ClassEdge
  deriving (Show, Eq, Ord)

xmlClass :: Class -> String
xmlClass ClassNode = "node"
xmlClass ClassEdge = "edge"
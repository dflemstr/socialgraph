{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.AttributeDecl where

import Text.XML.HXT.Arrow.Pickle
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

xpAttributeDecl :: Id a => PU (AttributeDecl a)
xpAttributeDecl =
  xpElem "attributes" $
  xpWrap (uncurry3 AttributeDecl,
          \ decl -> (declClass decl, declMode decl, declAttrs decl)
         ) $
  xpTriple
  (xpAttr "class" xpClass)
  (xpOption $ xpAttr "mode" xpMode)
  xpAttributes

xpAttributes :: Id a => PU [Attribute a]
xpAttributes =
  xpList xpAttribute

data Class =
  ClassNode |
  ClassEdge
  deriving (Show, Eq, Ord)

xpClass :: PU Class
xpClass =
  xpWrapMaybe (\ v -> case v of
                  "node" -> Just ClassNode
                  "edge" -> Just ClassEdge
                  _ -> Nothing,
               \ v -> case v of
                 ClassNode -> "node"
                 ClassEdge -> "edge"
              ) xpTText
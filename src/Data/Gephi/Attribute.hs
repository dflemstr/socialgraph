{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Attribute where

import Text.XML.HXT.Arrow.Pickle
import Data.Text (Text)
import Data.Gephi.Id
import Data.Gephi.Util

data Attribute a =
  Attribute { attrId :: a
            , attrTitle :: Text
            , attrType :: AttrType
            , attrDefault :: Maybe Text
            , attrOptions :: Maybe Text
            }
  deriving (Show, Eq, Ord)

xpAttribute :: Id a => PU (Attribute a)
xpAttribute =
  xpElem "attribute" $
  xpWrap (uncurry5 Attribute,
          \ attr -> (attrId attr, attrTitle attr, attrType attr,
                     attrDefault attr, attrOptions attr)
         ) $
  xp5Tuple
  (xpAttr "id" xpId)
  (xpAttr "title" xpTText)
  (xpAttr "type" xpType)
  (xpOption $ xpElem "default" xpTText)
  (xpOption $ xpElem "options" xpTText)

data AttrType =
  AttrInteger |
  AttrLong |
  AttrDouble |
  AttrFloat |
  AttrBoolean |
  AttrListString |
  AttrString |
  AttrAnyURI
  deriving (Show, Eq, Ord)

xpType :: PU AttrType
xpType =
  xpWrapMaybe (\ v -> case v of
                  "integer" -> Just AttrInteger
                  "long" -> Just AttrLong
                  "double" -> Just AttrDouble
                  "float" -> Just AttrFloat
                  "boolean" -> Just AttrBoolean
                  "liststring" -> Just AttrListString
                  "string" -> Just AttrString
                  "anyURI" -> Just AttrAnyURI
                  _ -> Nothing,
               \ v -> case v of
                 AttrInteger -> "integer"
                 AttrLong -> "long"
                 AttrDouble -> "double"
                 AttrFloat -> "float"
                 AttrBoolean -> "boolean"
                 AttrListString -> "liststring"
                 AttrString -> "string"
                 AttrAnyURI -> "anyURI"
              ) xpTText
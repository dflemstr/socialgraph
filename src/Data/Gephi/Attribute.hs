{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Attribute where

import Text.XML.Light
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
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

xmlAttribute :: Id a => Attribute a -> Element
xmlAttribute attr =
  Element (unqualified "attribute") [
    attribute "id" . xmlId . attrId $ attr,
    attribute "title" . Text.unpack . attrTitle $ attr,
    attribute "type" . xmlType . attrType $ attr]
  (Maybe.catMaybes [
      fmap (Elem . element "default" . Text.unpack) $ attrDefault attr,
      fmap (Elem . element "options" . Text.unpack) $ attrOptions attr])
  Nothing

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

xmlType :: AttrType -> String
xmlType AttrInteger = "integer"
xmlType AttrLong = "long"
xmlType AttrDouble = "double"
xmlType AttrFloat = "float"
xmlType AttrBoolean = "boolean"
xmlType AttrListString = "liststring"
xmlType AttrString = "string"
xmlType AttrAnyURI = "anyURI"
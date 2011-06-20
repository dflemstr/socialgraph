{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Attvalue where

import Text.XML.Light
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Gephi.Id
import Data.Gephi.Util

data Attvalue a =
  Attvalue { attvalueFor :: a
           , attvalueValue :: Text
           }
  deriving (Show, Eq, Ord)

xmlAttvalue :: Id a => Attvalue a -> Element
xmlAttvalue att =
  Element (unqualified "attvalue") [
    Attr (unqualified "for") $ xmlId $ attvalueFor att,
    Attr (unqualified "value") $ Text.unpack $ attvalueValue att
    ] [] Nothing

xmlAttvalues :: Id a => [Attvalue a] -> Element
xmlAttvalues atts =
  Element (unqualified "attvalues") [] (map (Elem . xmlAttvalue) atts) Nothing

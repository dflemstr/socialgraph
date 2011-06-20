{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Node where

import qualified Text.XML.Light as XML
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Gephi.Attvalue
import Data.Gephi.Id
import Data.Gephi.Util
import Data.Hashable

data Node a =
  Node { nodeId :: a
       , nodeLabel :: Text
       , nodeAttvalues :: Maybe [Attvalue a]
       }
  deriving (Show, Eq, Ord)

instance Hashable a => Hashable (Node a) where
  hash = hash . nodeId
  hashWithSalt salt = hashWithSalt salt . nodeId

xmlNode :: Id a => Node a -> XML.Element
xmlNode node =
  XML.Element (unqualified "node") [
    XML.Attr (unqualified "id") . xmlId . nodeId $ node,
    XML.Attr (unqualified "label") . Text.unpack . nodeLabel $ node]
  (maybe [] ((:[]) . XML.Elem . xmlAttvalues) $ nodeAttvalues node) Nothing

{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Gexf where

import qualified Text.XML.Light as XML
import qualified Data.Maybe as Maybe
import Data.Gephi.Meta
import Data.Gephi.Graph
import Data.Gephi.Id
import Data.Gephi.Util

data Gexf a =
  Gexf { gexfMeta :: Maybe Meta
       , gexfGraph :: Graph a
       }
  deriving (Show, Eq)

xmlGexf :: Id a => Gexf a -> XML.Element
xmlGexf gexf =
  XML.Element (unqualified "gexf")
  [ attribute "xmlns" "http://www.gexf.net/1.2draft"
  , attribute "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance"
  , attribute "xsi:schemaLocation" "http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd"
  , attribute "version" "1.2"
  ]
  (Maybe.catMaybes
   [ fmap (XML.Elem . xmlMeta) $ gexfMeta gexf
   , Just . XML.Elem . xmlGraph . gexfGraph $ gexf
   ]
  ) Nothing
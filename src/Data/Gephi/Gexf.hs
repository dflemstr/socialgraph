{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Gexf where

import Control.Arrow
import Text.XML.HXT.Arrow.Pickle
import Data.Gephi.Meta
import Data.Gephi.Graph
import Data.Gephi.Id

data Gexf a =
  Gexf { gexfMeta :: Maybe Meta
       , gexfGraph :: Graph a
       }
  deriving (Show, Eq)

xpGexf :: Id a => PU (Gexf a)
xpGexf =
  xpElem "gexf" $
  xpAddFixedAttr "xmlns" "http://www.gexf.net/1.2draft" $
  xpAddFixedAttr "xmlns:xsi" "http://www.w3.org/2001/XMLSchema-instance" $
  xpAddFixedAttr "xsi:schemaLocation" "http://www.gexf.net/1.2draft http://www.gexf.net/1.2draft/gexf.xsd" $
  xpAddFixedAttr "version" "1.2" $
  xpWrap (uncurry Gexf, gexfMeta &&& gexfGraph) $
  xpPair (xpOption xpMeta) xpGraph
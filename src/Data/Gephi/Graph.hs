{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Data.Gephi.Graph where

import Control.Arrow
import Text.XML.HXT.Arrow.Pickle
import qualified Data.Text as Text
import Data.Gephi.Edge
import Data.Gephi.Node
import Data.Gephi.Mode
import Data.Gephi.AttributeDecl
import Data.Gephi.Id
import Data.Gephi.Util

data Graph a =
  Graph { graphDefaultEdgeType :: EdgeType
        , graphMode :: Maybe Mode
        , graphNodes :: [Node a]
        , graphEdges :: [Edge a]
        , graphAttributeDecls :: [AttributeDecl a]
        }
  deriving (Show, Eq)

xpGraph :: forall a. Id a => PU (Graph a)
xpGraph =
  xpElem "graph" $
  xpWrap (uncurry5 Graph,
          \ graph -> (graphDefaultEdgeType graph,
                      graphMode graph, graphNodes graph, graphEdges graph,
                      graphAttributeDecls graph)
         ) $
  xpAddFixedAttr "idtype" (Text.unpack . gephiIdType $ (undefined :: a)) $
  xp5Tuple
  (xpAttr "defaultedgetype" xpEdgeType)
  (xpOption $ xpAttr "mode" xpMode) xpNodes xpEdges xpAttributeDecls

xpNodes :: Id a => PU [Node a]
xpNodes =
  xpElem "nodes" $
  xpWrap ( snd,
           length &&& id
         ) $
  xpPair
  (xpAttr "count" xpInt)
  (xpList xpNode)

xpEdges :: Id a => PU [Edge a]
xpEdges =
  xpElem "edges" $
  xpWrap ( snd,
           length &&& id
         ) $
  xpPair
  (xpAttr "count" xpInt)
  (xpList xpEdge)

xpAttributeDecls :: Id a => PU [AttributeDecl a]
xpAttributeDecls =
  xpList xpAttributeDecl
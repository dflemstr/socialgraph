{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}
module Data.Gephi.Graph where

import qualified Text.XML.Light as XML
import qualified Data.Text as Text
import qualified Data.Maybe as Maybe
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

xmlGraph :: forall a. Id a => Graph a -> XML.Element
xmlGraph graph =
  XML.Element
  (unqualified "graph")
  (Maybe.catMaybes [
      Just $ attribute "idtype" (Text.unpack . gephiIdType $ (undefined :: a)),
      Just . attribute "defaultedgetype" . xmlEdgeType . graphDefaultEdgeType $ graph,
      fmap (attribute "mode" . xmlMode) $ graphMode graph])
  (map XML.Elem ((xmlNodes . graphNodes $ graph) :
                 (xmlEdges . graphEdges $ graph) :
                 (xmlAttributeDecls . graphAttributeDecls $ graph))) Nothing

xmlNodes :: Id a => [Node a] -> XML.Element
xmlNodes nodes =
  XML.Element
  (unqualified "nodes")
  []
  (map (XML.Elem . xmlNode) nodes) Nothing

xmlEdges :: Id a => [Edge a] -> XML.Element
xmlEdges edges =
  XML.Element
  (unqualified "edges")
  []
  (map (XML.Elem . xmlEdge) edges) Nothing

xmlAttributeDecls :: Id a => [AttributeDecl a] -> [XML.Element]
xmlAttributeDecls = map xmlAttributeDecl
{-# LANGUAGE OverloadedStrings #-}
module Data.SocialGraph.Convert (graphToGexf) where

import Control.Monad.State
import qualified Data.Gephi as Gephi
import Data.SocialGraph.Graph (Graph)
import qualified Data.SocialGraph.Graph as Graph
import qualified Data.SocialGraph.Node as Node
import qualified Data.SocialGraph.Edge as Edge
import qualified Data.SocialGraph.Identity as Identity
import qualified Data.Text as Text
import qualified Data.HashSet as Set
import Data.StringCache (StringCache)
import qualified Data.StringCache as StringCache
import qualified Data.Maybe as Maybe

graphToGexf :: Monad m => Graph -> StateT StringCache m (Gephi.Gexf Int)
graphToGexf graph = do
  g <- graphToGephiGraph graph
  return Gephi.Gexf { Gephi.gexfMeta = Just gexfMetadata
                    , Gephi.gexfGraph = g
                    }

graphToGephiGraph :: Monad m => Graph -> StateT StringCache m (Gephi.Graph Int)
graphToGephiGraph graph = do
  edges <- mapM makeGephiEdge . Set.toList $ Graph.edges graph
  let decls = [edgeAttributeDecl]
      nodes = map makeGephiNode . Set.toList $ Graph.nodes graph
  return Gephi.Graph { Gephi.graphDefaultEdgeType = Gephi.EdgeDirected
                     , Gephi.graphMode = Just Gephi.ModeStatic
                     , Gephi.graphNodes = nodes
                     , Gephi.graphEdges = edges
                     , Gephi.graphAttributeDecls = decls
                     }

makeGephiNode :: Node.Node -> Gephi.Node Int
makeGephiNode node = Gephi.Node { Gephi.nodeId = Node.key node
                                , Gephi.nodeLabel = Identity.display . Node.identity $ node
                                , Gephi.nodeAttvalues = Nothing
                                }

makeGephiEdge :: Monad m => Edge.Edge -> StateT StringCache m (Gephi.Edge Int)
makeGephiEdge edge = do
  edgeId <- StringCache.wasteId
  return Gephi.Edge { Gephi.edgeId = edgeId
                    , Gephi.edgeType = Just Gephi.EdgeDirected
                    , Gephi.edgeLabel = Maybe.listToMaybe kindNames
                    , Gephi.edgeSource = Edge.fromNode edge
                    , Gephi.edgeTarget = Edge.toNode edge
                    , Gephi.edgeWeight = Nothing
                    , Gephi.edgeAttvalues = if null attvalues then Nothing else Just attvalues
                    }
  where
    attvalues = [Gephi.Attvalue 0 $ Text.intercalate "|" kindNames]
    kindNames = map Edge.edgeKindName . Edge.kinds $ edge

edgeAttributeDecl :: Gephi.AttributeDecl Int
edgeAttributeDecl =
  Gephi.AttributeDecl { Gephi.declClass = Gephi.ClassEdge
                      , Gephi.declMode = Just Gephi.ModeStatic
                      , Gephi.declAttrs = [attr]
                      }
    where
      attr = Gephi.Attribute { Gephi.attrId = 0
                             , Gephi.attrTitle = "Relationship types"
                             , Gephi.attrType = Gephi.AttrListString
                             , Gephi.attrDefault = Nothing
                             , Gephi.attrOptions = Just "me|contact|acuqiantance|friend|met|co-worker|colleague|co-resident|neighbor|child|parent|sibling|spouse|kin|muse|crush|date|sweetheart"
                             }

gexfMetadata :: Gephi.Meta
gexfMetadata =
  Gephi.Meta { Gephi.metaLastModifiedDate = Nothing
             , Gephi.metaCreator = Just "socialgraph version 0.1"
             , Gephi.metaKeywords = Just "google, socialgraph, social, graph"
             , Gephi.metaDescription = Just
               "Shows relationships between identities using the Google \
               \SocialGraph API"
             }

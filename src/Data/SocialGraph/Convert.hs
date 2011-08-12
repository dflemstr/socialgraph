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
import qualified Data.HashMap.Strict as HashMap
import qualified Data.Maybe as Maybe

graphToGexf :: Monad m => Graph -> Gephi.Gexf Int
graphToGexf graph = do
  g <- graphToGephiGraph graph
  return Gephi.Gexf { Gephi.gexfMeta = Just gexfMetadata
                    , Gephi.gexfGraph = g
                    }

graphToGephiGraph :: Monad m => Graph -> Gephi.Graph Int
graphToGephiGraph graph = do
  edges <- mapM (\ ((f,t),v) -> makeGephiEdge f t v) . HashMap.toList $ Graph.edges graph
  let decls = [nodeAttributeDecl, edgeAttributeDecl]
      nodes = map (uncurry makeGephiNode) . HashMap.toList $ Graph.nodes graph
  return Gephi.Graph { Gephi.graphDefaultEdgeType = Gephi.EdgeDirected
                     , Gephi.graphMode = Just Gephi.ModeStatic
                     , Gephi.graphNodes = nodes
                     , Gephi.graphEdges = edges
                     , Gephi.graphAttributeDecls = decls
                     }

makeGephiNode :: Int -> Node.Node -> Gephi.Node Int
makeGephiNode key node =
  Gephi.Node { Gephi.nodeId = key
             , Gephi.nodeLabel = Identity.display . Node.identity $ node
             , Gephi.nodeAttvalues = if null attvalues then Nothing else Just attvalues
             }
  where
    attvalues = mkAttvalues . Node.attributes $ node
    mkAttvalues =
      (++ [ Gephi.Attvalue incomingAttrId . Text.pack . show . Node.directInConnections $ node
          , Gephi.Attvalue outgoingAttrId . Text.pack . show . Node.directOutConnections $ node
          , Gephi.Attvalue iterationAttrId . Text.pack . show . Node.iteration $ node
          ]
      ) .
      map (\ (kind, text) -> Gephi.Attvalue (fromEnum kind) text)

makeGephiEdge :: Monad m => Int -> Int -> Edge.Edge -> StateT StringCache m (Gephi.Edge Int)
makeGephiEdge fromKey toKey edge = do
  edgeId <- StringCache.wasteId
  return Gephi.Edge { Gephi.edgeId = edgeId
                    , Gephi.edgeType = Just Gephi.EdgeDirected
                    , Gephi.edgeLabel = Maybe.listToMaybe kindNames
                    , Gephi.edgeSource = fromKey
                    , Gephi.edgeTarget = toKey
                    , Gephi.edgeWeight = Nothing
                    , Gephi.edgeAttvalues = if null attvalues then Nothing else Just attvalues
                    }
  where
    attvalues = [Gephi.Attvalue 0 $ Text.intercalate "|" kindNames]
    kindNames = map Edge.edgeKindName . Edge.edgeKinds $ edge

incomingAttrId :: Int
incomingAttrId = fromEnum (maxBound :: Node.AttributeKind) + 1

outgoingAttrId :: Int
outgoingAttrId = fromEnum (maxBound :: Node.AttributeKind) + 2

iterationAttrId :: Int
iterationAttrId = fromEnum (maxBound :: Node.AttributeKind) + 3

nodeAttributeDecl :: Gephi.AttributeDecl Int
nodeAttributeDecl =
  Gephi.AttributeDecl { Gephi.declClass = Gephi.ClassNode
                      , Gephi.declMode = Just Gephi.ModeStatic
                      , Gephi.declAttrs = attrs
                      }
  where
    attrs = map mkAttr [minBound .. maxBound] ++ [incomingAttr, outgoingAttr, iterationAttr]
    mkAttr :: Node.AttributeKind -> Gephi.Attribute Int
    mkAttr kind =
      Gephi.Attribute { Gephi.attrId = fromEnum kind
                      , Gephi.attrTitle = Text.pack . show $ kind
                      , Gephi.attrType = Gephi.AttrString
                      , Gephi.attrDefault = Nothing
                      , Gephi.attrOptions = Nothing
                      }
    incomingAttr =
      Gephi.Attribute { Gephi.attrId = incomingAttrId
                      , Gephi.attrTitle = "Incoming connections"
                      , Gephi.attrType = Gephi.AttrInteger
                      , Gephi.attrDefault = Just "0"
                      , Gephi.attrOptions = Nothing
                      }
    outgoingAttr =
      Gephi.Attribute { Gephi.attrId = outgoingAttrId
                      , Gephi.attrTitle = "Outgoing connections"
                      , Gephi.attrType = Gephi.AttrInteger
                      , Gephi.attrDefault = Just "0"
                      , Gephi.attrOptions = Nothing
                      }
    iterationAttr =
      Gephi.Attribute { Gephi.attrId = iterationAttrId
                      , Gephi.attrTitle = "Iteration"
                      , Gephi.attrType = Gephi.AttrInteger
                      , Gephi.attrDefault = Just "0"
                      , Gephi.attrOptions = Nothing
                      }

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

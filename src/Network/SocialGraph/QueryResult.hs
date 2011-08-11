{-# LANGUAGE OverloadedStrings #-}
module Network.SocialGraph.QueryResult
       ( QueryGraph(..)
       , QueryNode(..)
       , QueryEdge(..)
       , toGraph
       ) where

import Control.Applicative
import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.Word
import Data.Aeson
import Data.Text (Text)
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.StringCache (StringCache)
import qualified Data.StringCache as StringCache
import Data.SocialGraph.Node (Node)
import qualified Data.SocialGraph.Node as Node
import Data.SocialGraph.Edge (Edge)
import qualified Data.SocialGraph.Edge as Edge
import Data.SocialGraph.Graph (Graph)
import qualified Data.SocialGraph.Graph as Graph
import qualified Data.SocialGraph.Identity as Identity
import qualified Data.Maybe as Maybe

data QueryGraph =
  QueryGraph { canonicalHashMapping :: HashMap Text Text
             , nodes :: HashMap Text QueryNode
             }
  deriving (Show, Eq)

data QueryNode =
  QueryNode { attributes :: HashMap Text Text
            , nodesReferenced :: Maybe (HashMap Text QueryEdge)
            , nodesReferencedBy :: Maybe (HashMap Text QueryEdge)
            }
  deriving (Show, Eq)

data QueryEdge =
  QueryEdge { types :: [Text]
            }
  deriving (Show, Eq)

toGraph :: Monad m => Word -> Bool -> QueryGraph -> StateT StringCache m Graph
toGraph iter close qresult = do
  nodesSC <- storeNodes iter $ nodes qresult
  edgesSC <- storeEdges . HashMap.map fst $ nodesSC
  let pureGraph = Graph.Graph { Graph.nodes = HashMap.map snd nodesSC
                              , Graph.edges = HashMap.map toEdge edgesSC
                              }
  if close
    then return pureGraph
    else Graph.addGhostNodes iter pureGraph

toNode :: Word -> Text -> QueryNode -> Node
toNode iter url qnode =
  Node.Node { Node.identity = Identity.make url
            , Node.attributes = attrs
            , Node.directOutConnections = 0
            , Node.directInConnections = 0
            , Node.iteration = iter
            }
  where
    attrs = do
      (kindName, value) <- HashMap.toList . attributes $ qnode
      case Node.identifyAttributeKind kindName of
        Just kind -> return (kind, value)
        Nothing -> mzero

toEdge :: QueryEdge -> Edge
toEdge qedge =
  Edge.Edge { Edge.edgeKinds = kinds }
  where
    kinds = Maybe.mapMaybe Edge.identifyEdgeKind . types $ qedge

storeNodes :: Monad m
              => Word -> HashMap Text QueryNode
              -> StateT StringCache m (HashMap Int (QueryNode, Node))
storeNodes iter nodeHashMap = do
  let assocs = map (\ (k, v) -> (k, (v, toNode iter k v))) $ HashMap.toList nodeHashMap
  assocsSC <- mapM (doLeft StringCache.storeString) assocs
  return $ HashMap.fromList assocsSC

storeEdges :: Monad m
              => HashMap Int QueryNode
              -> StateT StringCache m (HashMap (Int, Int) QueryEdge)
storeEdges nodesSC = do
  edgesd <- mapM combine edges
  return . HashMap.fromList $ concat edgesd
  where
    edges = map insAndOuts . HashMap.toList $ nodesSC
    insAndOuts = uncurry storeEdgesOut &&& uncurry storeEdgesIn
    combine (a, b) = do
      as <- a
      bs <- b
      return $ as ++ bs

storeEdgesOut :: Monad m
                 => Int
                 -> QueryNode
                 -> StateT StringCache m [((Int, Int), QueryEdge)]
storeEdgesOut inKey node = do
  let assocs = HashMap.toList =<< (Maybe.maybeToList . nodesReferenced $ node)
  assocsSC <- mapM (doLeft StringCache.storeString) assocs
  return $ map regroup assocsSC
  where
    regroup (outKey, edge) = ((inKey, outKey), edge)

storeEdgesIn :: Monad m
                => Int
                -> QueryNode
                -> StateT StringCache m [((Int, Int), QueryEdge)]
storeEdgesIn outKey node = do
  let assocs = HashMap.toList =<< (Maybe.maybeToList . nodesReferencedBy $ node)
  assocsSC <- mapM (doLeft StringCache.storeString) assocs
  return $ map regroup assocsSC
  where
    regroup (inKey, edge) = ((inKey, outKey), edge)

doLeft :: Monad m => (a -> m b) -> (a, c) -> m (b, c)
doLeft f (l, r) = do
  nl <- f l
  return (nl, r)

instance FromJSON QueryGraph where
  parseJSON (Object o) =
    QueryGraph
    <$> o .: "canonical_mapping"
    <*> o .: "nodes"
  parseJSON _ = mzero

instance FromJSON QueryNode where
  parseJSON (Object o) =
    QueryNode
    <$> o .: "attributes"
    <*> o .:? "nodes_referenced"
    <*> o .:? "nodes_referenced_by"
  parseJSON _ = mzero

instance FromJSON QueryEdge where
  parseJSON (Object o) =
    QueryEdge
    <$> o .: "types"
  parseJSON _ = mzero

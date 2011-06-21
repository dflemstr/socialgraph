{-# LANGUAGE OverloadedStrings #-}
module Network.SocialGraph.QueryResult 
       ( QueryResult(..)
       , QueryNode(..)
       , QueryEdge(..)
       , toGraph
       ) where

import Control.Applicative
import Control.Monad
import Control.Monad.State
import Data.Aeson
import Data.Text (Text)
import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.HashSet as Set
import Data.StringCache (StringCache)
import qualified Data.StringCache as StringCache
import qualified Data.SocialGraph.Node as Node
import qualified Data.SocialGraph.Edge as Edge
import Data.SocialGraph.Graph (Graph)
import qualified Data.SocialGraph.Graph as Graph
import qualified Data.SocialGraph.Identity as Identity
import qualified Data.Maybe as Maybe

data QueryResult =
  QueryResult { canonicalMapping :: Map Text Text
              , nodes :: Map Text QueryNode
              }
  deriving (Show, Eq)

data QueryNode =
  QueryNode { attributes :: Map Text Text
            , nodesReferenced :: Maybe (Map Text QueryEdge)
            , nodesReferencedBy :: Maybe (Map Text QueryEdge)
            }
  deriving (Show, Eq)

data QueryEdge =
  QueryEdge { types :: [Text]
            }
  deriving (Show, Eq)

filterEmptyList :: Maybe [k] -> [k]
filterEmptyList (Nothing) = []
filterEmptyList (Just ls) = ls

toGraph :: Monad m => QueryResult -> StateT StringCache m Graph
toGraph qresult = do
  resultNodes <- mapM makeNode urls
  resultEdges <- mapM (uncurry3 makeEdge) edgeData
  Graph.addGhostNodes $ Graph.Graph (Set.fromList resultNodes) (Set.fromList resultEdges)
    where
      qnodes = nodes qresult
      urls = Map.keys qnodes
      referencedOf =
        filterEmptyList . fmap Map.keys . nodesReferenced . (Map.!) qnodes
      referencedByOf =
        filterEmptyList . fmap Map.keys . nodesReferencedBy . (Map.!) qnodes
      typesOf nodeName references outerName =
        let node = (Map.!) qnodes nodeName
            maybeRefs = references node
        in case maybeRefs of
          Nothing -> []
          Just refs ->
            let edge = (Map.!) refs outerName
            in types edge
      edgeData = incomingEdgeData ++ outgoingEdgeData
      incomingEdgeData = do
        node  <- urls
        from <- referencedByOf node
        return (from, node, typesOf node nodesReferencedBy from)
      outgoingEdgeData = do
        node  <- urls
        to <- referencedOf node
        return (node, to, typesOf node nodesReferenced to)

makeNode :: Monad m => Text -> StateT StringCache m Node.Node
makeNode url = do
  key <- StringCache.storeString url
  return Node.Node { Node.key = key
                   , Node.identity = Identity.make url
                   }

makeEdge :: Monad m => Text -> Text -> [Text] -> StateT StringCache m Edge.Edge
makeEdge from to ts = do
  keyFrom <- StringCache.storeString from
  keyTo <- StringCache.storeString to
  return Edge.Edge { Edge.fromNode = keyFrom
                   , Edge.toNode = keyTo
                   , Edge.kinds = kinds
                   }
    where kinds = Maybe.mapMaybe Edge.identifyEdgeKind ts

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f1 (a, b, c) = f1 a b c

instance FromJSON QueryResult where
  parseJSON (Object o) =
    QueryResult
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

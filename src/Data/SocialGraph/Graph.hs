module Data.SocialGraph.Graph
       ( Graph(..)
       , merge
       , empty
       , addGhostNodes
       ) where

import Control.Monad.State
import Data.SocialGraph.Node (Node)
import qualified Data.SocialGraph.Node as Node
import Data.SocialGraph.Edge (Edge)
import qualified Data.SocialGraph.Edge as Edge
import qualified Data.SocialGraph.Identity as Identity
import Data.HashSet (Set)
import qualified Data.HashSet as Set
import Data.StringCache (StringCache)
import qualified Data.StringCache as StringCache

data Graph =
  Graph { nodes :: Set Node
        , edges :: Set Edge
        }
  deriving (Show, Eq)

merge :: Graph -> Graph -> Graph
merge graph1 graph2 =
  Graph { nodes = nodes graph1 `Set.union` nodes graph2
        , edges = edges graph1 `Set.union` edges graph2
        }

empty :: Graph
empty = Graph Set.empty Set.empty

addGhostNodes :: Monad m => Graph -> StateT StringCache m Graph
addGhostNodes graph = do
  ghostNodes <- mapM makeNode $ Set.toList unregistered
  return Graph { nodes = nodes graph `Set.union` Set.fromList ghostNodes
               , edges = edges graph
               }
  where
    es = edges graph
    outgoing = Set.map Edge.toNode es
    incoming = Set.map Edge.fromNode es
    allExternal = outgoing `Set.union` incoming
    registered = Set.map Node.key $ nodes graph
    unregistered = allExternal `Set.difference` registered
    makeNode k = do
      url <- StringCache.getString k
      return Node.Node { Node.key = k
                       , Node.identity = Identity.make url
                       }

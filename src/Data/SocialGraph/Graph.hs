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
import qualified Data.SocialGraph.Identity as Identity
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import qualified Data.IntSet as IntSet
import Data.StringCache (StringCache)
import qualified Data.StringCache as StringCache

data Graph =
  Graph { nodes :: HashMap Key Node
        , edges :: HashMap (Key, Key) Edge
        }
  deriving (Show, Eq)

type Key = Int

merge :: Graph -> Graph -> Graph
merge graph1 graph2 =
  Graph { nodes = nodes graph1 `HashMap.union` nodes graph2
        , edges = edges graph1 `HashMap.union` edges graph2
        }

empty :: Graph
empty = Graph HashMap.empty HashMap.empty

addGhostNodes :: Monad m => Graph -> StateT StringCache m Graph
addGhostNodes graph = do
  ghostNodes <- mapM makeNode $ IntSet.toList unregistered
  return Graph { nodes = nodes graph `HashMap.union` HashMap.fromList ghostNodes
               , edges = edges graph
               }
  where
    keys = HashMap.keys . edges $ graph
    outgoing = IntSet.fromList . map snd $ keys
    incoming = IntSet.fromList . map fst $ keys
    allExternal = outgoing `IntSet.union` incoming
    registered = IntSet.fromList . HashMap.keys . nodes $ graph
    unregistered = allExternal `IntSet.difference` registered
    makeNode k = do
      url <- StringCache.getString k
      return (k,
              Node.Node { Node.identity = Identity.make url
                        , Node.attributes = []
                        })

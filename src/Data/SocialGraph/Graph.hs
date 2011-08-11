module Data.SocialGraph.Graph
       ( Graph(..)
       , merge
       , empty
       , addGhostNodes
       , cleanEdges
       , countEdges
       ) where

import Control.Monad.State
import Control.Arrow
import Data.Word (Word)
import Data.SocialGraph.Node (Node)
import qualified Data.SocialGraph.Node as Node
import Data.SocialGraph.Edge (Edge)
import qualified Data.SocialGraph.Identity as Identity
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap
import Data.IntMap (IntMap)
import qualified Data.IntMap as IntMap
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

cleanEdges :: Graph -> Graph
cleanEdges g =
  g { edges = HashMap.filterWithKey (\ (k1, k2) _ -> nodeExists k1 && nodeExists k2) $ edges g }
  where
    nodeExists = flip IntSet.member keys
    keys = IntSet.fromList . HashMap.keys . nodes $ g

countEdges :: Graph -> Graph
countEdges g =
  g { nodes = HashMap.fromList . map (uncurry addTotals) . HashMap.toList $ nodes g}
  where
    addTotals k n = (k,
                     n { Node.directInConnections = IntMap.findWithDefault 0 k incoming
                       , Node.directOutConnections = IntMap.findWithDefault 0 k outgoing
                       }
                    )
    (outgoing, incoming) = execState (doCountEdges . HashMap.keys . edges $ g) (IntMap.empty, IntMap.empty)

doCountEdges :: [(Key, Key)] -> State (IntMap Word, IntMap Word) ()
doCountEdges []              = return ()
doCountEdges ((i, o) : rest) = do
  modify $ addKey i *** addKey o
  doCountEdges rest
  where
    addKey k = IntMap.insertWith (+) k 1

addGhostNodes :: Monad m => Word -> Graph -> StateT StringCache m Graph
addGhostNodes iter graph = do
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
                        , Node.directInConnections = 0
                        , Node.directOutConnections = 0
                        , Node.iteration = iter
                        })

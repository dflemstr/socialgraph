{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Edge where

import Text.XML.HXT.Arrow.Pickle
import Data.Text (Text)
import Data.Gephi.Id
import Data.Gephi.Attvalue
import Data.Gephi.Util
import Data.Hashable

data Edge a =
  Edge { edgeId :: a
       , edgeType :: Maybe EdgeType
       , edgeLabel :: Maybe Text
       , edgeSource :: a
       , edgeTarget :: a
       , edgeWeight :: Maybe Rational
       , edgeAttvalues :: Maybe [Attvalue a]
       }
  deriving (Show, Eq, Ord)

instance Hashable a => Hashable (Edge a) where
  hash = hash . edgeId
  hashWithSalt salt = hashWithSalt salt . edgeId

xpEdge :: Id a => PU (Edge a)
xpEdge =
  xpElem "edge" $
  xpWrap (uncurry7 Edge,
          \ e -> (edgeId e, edgeType e, edgeLabel e,
                  edgeSource e, edgeTarget e, edgeWeight e,
                  edgeAttvalues e)
         ) $
  xp7Tuple
  (xpAttr "id" xpId)
  (xpOption $ xpAttr "type" xpEdgeType)
  (xpOption $ xpAttr "label" xpTText)
  (xpAttr "source" xpId)
  (xpAttr "target" xpId)
  (xpOption $ xpAttr "weight" xpAny)
  (xpOption xpAttvalues)

data EdgeType =
  EdgeDirected |
  EdgeUndirected |
  EdgeMutual
  deriving (Show, Eq, Ord)

xpEdgeType :: PU EdgeType
xpEdgeType =
  xpWrapMaybe (\ v -> case v of
                  "directed" -> Just EdgeDirected
                  "undirected" -> Just EdgeUndirected
                  "mutual" -> Just EdgeMutual
                  _ -> Nothing,
               \ v -> case v of
                 EdgeDirected -> "directed"
                 EdgeUndirected -> "undirected"
                 EdgeMutual -> "mutual"
              ) xpTText
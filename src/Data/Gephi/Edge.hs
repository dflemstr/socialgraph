{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Edge where

import Text.XML.Light
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Gephi.Id
import Data.Gephi.Attvalue
import Data.Gephi.Util
import qualified Data.Maybe as Maybe

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

xmlEdge :: Id a => Edge a -> Element
xmlEdge edge =
  Element
  (unqualified "edge")
  (Maybe.catMaybes [
      Just . attribute "id" . xmlId . edgeId $ edge,
      fmap (attribute "type" . xmlEdgeType) $ edgeType edge,
      fmap (attribute "label" . Text.unpack) $ edgeLabel edge,
      Just . attribute "source" . xmlId . edgeSource $ edge,
      Just . attribute "target" . xmlId . edgeTarget $ edge,
      fmap (attribute "weight" . show) $ edgeWeight edge])
  (maybe [] ((:[]) . Elem . xmlAttvalues) $ edgeAttvalues edge) Nothing

data EdgeType =
  EdgeDirected |
  EdgeUndirected |
  EdgeMutual
  deriving (Show, Eq, Ord)

xmlEdgeType :: EdgeType -> String
xmlEdgeType EdgeDirected = "directed"
xmlEdgeType EdgeUndirected = "undirected"
xmlEdgeType EdgeMutual = "mutual"
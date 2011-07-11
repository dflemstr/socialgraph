{-# LANGUAGE OverloadedStrings #-}
module Data.SocialGraph.Edge where

import Data.Text (Text)

data Edge =
  Edge { edgeKinds :: ![EdgeKind]
       } deriving (Show, Eq, Ord)

data EdgeKind =
  Me |
  Contact |
  Acquaintance |
  Friend |
  Met |
  CoWorker |
  Colleague |
  CoResident |
  Neighbor |
  Child |
  Parent |
  Sibling |
  Spouse |
  Kin |
  Muse |
  Crush |
  Date |
  Sweetheart
  deriving (Show, Eq, Ord)

edgeKindName :: EdgeKind -> Text
edgeKindName Me = "me"
edgeKindName Contact = "contact"
edgeKindName Acquaintance = "acquaintance"
edgeKindName Friend = "friend"
edgeKindName Met = "met"
edgeKindName CoWorker = "co-worker"
edgeKindName Colleague = "colleague"
edgeKindName CoResident = "co-resident"
edgeKindName Neighbor = "neighbor"
edgeKindName Child = "child"
edgeKindName Parent = "parent"
edgeKindName Sibling = "sibling"
edgeKindName Spouse = "spouse"
edgeKindName Kin = "kin"
edgeKindName Muse = "muse"
edgeKindName Crush = "crush"
edgeKindName Date = "date"
edgeKindName Sweetheart = "sweetheart"

identifyEdgeKind :: Text -> Maybe EdgeKind
identifyEdgeKind "me"           = Just Me
identifyEdgeKind "contact"      = Just Contact
identifyEdgeKind "acquaintance" = Just Acquaintance
identifyEdgeKind "friend"       = Just Friend
identifyEdgeKind "met"          = Just Met
identifyEdgeKind "co-worker"    = Just CoWorker
identifyEdgeKind "colleague"    = Just Colleague
identifyEdgeKind "co-resident"  = Just CoResident
identifyEdgeKind "neighbor"     = Just Neighbor
identifyEdgeKind "child"        = Just Child
identifyEdgeKind "parent"       = Just Parent
identifyEdgeKind "sibling"      = Just Sibling
identifyEdgeKind "spouse"       = Just Spouse
identifyEdgeKind "kin"          = Just Kin
identifyEdgeKind "muse"         = Just Muse
identifyEdgeKind "crush"        = Just Crush
identifyEdgeKind "date"         = Just Date
identifyEdgeKind "sweetheart"   = Just Sweetheart
identifyEdgeKind _ = Nothing
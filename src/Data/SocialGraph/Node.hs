{-# LANGUAGE OverloadedStrings #-}
module Data.SocialGraph.Node
       ( Node(..)
       , AttributeKind(..)
       , attributeKindName
       , identifyAttributeKind
       ) where

import Data.Word
import Data.SocialGraph.Identity (Identity)
import Data.Text (Text)

data Node =
  Node { identity :: !Identity
       , directOutConnections :: !Word
       , directInConnections :: !Word
       , iteration :: !Word
       , attributes :: [(AttributeKind, Text)]
       } deriving (Eq, Show, Ord)

data AttributeKind =
  URL |
  Profile |
  RSS |
  Atom |
  FOAF |
  Photo |
  Name
  deriving (Eq, Show, Ord, Enum, Bounded)

attributeKindName :: AttributeKind -> Text
attributeKindName URL = "url"
attributeKindName Profile = "profile"
attributeKindName RSS = "rss"
attributeKindName Atom = "atom"
attributeKindName FOAF = "foaf"
attributeKindName Photo = "photo"
attributeKindName Name = "name"

identifyAttributeKind :: Text -> Maybe AttributeKind
identifyAttributeKind "url"     = Just URL
identifyAttributeKind "profile" = Just Profile
identifyAttributeKind "rss"     = Just RSS
identifyAttributeKind "atom"    = Just Atom
identifyAttributeKind "foaf"    = Just FOAF
identifyAttributeKind "photo"   = Just Photo
identifyAttributeKind "name"    = Just Name
identifyAttributeKind _         = Nothing
{-# LANGUAGE OverloadedStrings #-}
module Data.SocialGraph.Node 
       ( Node(..)
       , Key
       ) where

import Data.SocialGraph.Identity (Identity)
import Data.Hashable

data Node =
  Node { key :: !Key
       , identity :: !Identity
       } deriving (Show)

instance Hashable Node where
  hash = hash . key
  hashWithSalt salt = hashWithSalt salt . key

instance Eq Node where
  Node k1 _ == Node k2 _ =
    k1 == k2

instance Ord Node where
  Node k1 _ `compare` Node k2 _ =
    k1 `compare` k2

type Key = Int
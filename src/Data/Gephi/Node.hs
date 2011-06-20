{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Node where

import Text.XML.HXT.Arrow.Pickle
import Data.Text (Text)
import Data.Gephi.Attvalue
import Data.Gephi.Id
import Data.Gephi.Util
import Data.Hashable

data Node a =
  Node { nodeId :: a
       , nodeLabel :: Text
       , nodeAttvalues :: Maybe [Attvalue a]
       }
  deriving (Show, Eq, Ord)

instance Hashable a => Hashable (Node a) where
  hash = hash . nodeId
  hashWithSalt salt = hashWithSalt salt . nodeId

xpNode :: Id a => PU (Node a)
xpNode =
  xpElem "node" $
  xpWrap (uncurry3 Node,
          \ n -> (nodeId n, nodeLabel n, nodeAttvalues n)
         ) $
  xpTriple
  (xpAttr "id" xpId)
  (xpAttr "label" xpTText)
  (xpOption xpAttvalues)
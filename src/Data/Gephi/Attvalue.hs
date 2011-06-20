{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Attvalue where

import Control.Arrow
import Text.XML.HXT.Arrow.Pickle
import Data.Text (Text)
import Data.Gephi.Id
import Data.Gephi.Util

data Attvalue a =
  Attvalue { attvalueFor :: a
           , attvalueValue :: Text
           }
  deriving (Show, Eq, Ord)

xpAttvalue :: Id a => PU (Attvalue a)
xpAttvalue =
  xpElem "attvalue" $
  xpWrap (uncurry Attvalue, attvalueFor &&& attvalueValue) $
  xpPair
  (xpAttr "for" xpId)
  (xpAttr "value" xpTText)

xpAttvalues :: Id a => PU [Attvalue a]
xpAttvalues =
  xpElem "attvalues" $
  xpList xpAttvalue

{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Meta where

import Text.XML.HXT.Arrow.Pickle
import Data.Text (Text)
import Data.Gephi.Util

data Meta =
  Meta { metaLastModifiedDate :: Maybe Text
       , metaCreator :: Maybe Text
       , metaKeywords :: Maybe Text
       , metaDescription :: Maybe Text
       }
  deriving (Show, Eq)

xpMeta :: PU Meta
xpMeta =
  xpElem "meta" $
  xpWrap (uncurry4 Meta,
          \ meta -> (metaLastModifiedDate meta, metaCreator meta,
                     metaKeywords meta, metaDescription meta)
         ) $
  xp4Tuple
  (xpOption $ xpAttr "lastmodifieddate" xpTText)
  (xpOption $ xpElem "creator" xpTText)
  (xpOption $ xpElem "keywords" xpTText) 
  (xpOption $ xpElem "description" xpTText)

uncurry4 :: (a -> b -> c -> d -> e) -> (a, b, c, d) -> e
uncurry4 f (a, b, c, d) = f a b c d
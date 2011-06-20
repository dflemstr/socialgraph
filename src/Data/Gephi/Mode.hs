{-# LANGUAGE OverloadedStrings #-}
module Data.Gephi.Mode where

import Text.XML.HXT.Arrow.Pickle
import Data.Gephi.Util

data Mode =
  ModeStatic |
  ModeDynamic
  deriving (Show, Eq, Ord)

xpMode :: PU Mode
xpMode =
  xpWrapMaybe (\ v -> case v of
                  "static" -> Just ModeStatic
                  "dynamic" -> Just ModeDynamic
                  _ -> Nothing,
               \ v -> case v of
                 ModeStatic -> "static"
                 ModeDynamic -> "dynamic"
              ) xpTText
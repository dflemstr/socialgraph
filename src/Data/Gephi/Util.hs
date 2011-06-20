module Data.Gephi.Util where

import Text.XML.HXT.Arrow.Pickle
import Data.Text (Text)
import qualified Data.Text as Text

xpAny :: (Show a, Read a) => PU a
xpAny = xpWrap (read, show) xpText
   
xpTText :: PU Text
xpTText = xpWrap (Text.pack, Text.unpack) xpText

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f1 (a, b, c) = f1 a b c

uncurry5 :: (a -> b -> c -> d -> e -> f) -> (a, b, c, d, e) -> f
uncurry5 f1 (a, b, c, d, e) = f1 a b c d e

uncurry7 :: (a -> b -> c -> d -> e -> f -> g -> h) -> (a, b, c, d, e, f, g) -> h
uncurry7 f1 (a, b, c, d, e, f, g) =  f1 a b c d e f g
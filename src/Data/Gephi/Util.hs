module Data.Gephi.Util where

import Text.XML.Light

unqualified :: String -> QName
unqualified str = QName str Nothing Nothing

attribute :: String -> String -> Attr
attribute name value = Attr (unqualified name) value

element :: String -> String -> Element
element name value = Element (unqualified name) [] [Text (CData CDataText value Nothing)] Nothing

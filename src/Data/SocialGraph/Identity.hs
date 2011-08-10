{-# LANGUAGE OverloadedStrings #-}
module Data.SocialGraph.Identity
       ( Identity(..)
       , display
       , make
       ) where

import Prelude hiding (takeWhile)
import Control.Applicative
import Data.Text (Text)
import qualified Data.Text as Text
import Data.Attoparsec.Text
import Data.Int

data Identity =
  Ident { uri :: !Text
        , name :: !Text
        } |
  PK { uri :: !Text
     , pk :: !Int64
     } |
  URL { uri :: !Text
      } |
  URI { uri :: !Text
      }
  deriving (Show, Eq, Ord)

display :: Identity -> Text
display (Ident "twitter.com" tname) = "@" `Text.append` tname
display (Ident turi tname) = tname `Text.append` " on " `Text.append` turi
display (PK turi ipk) = "Database entry " `Text.append` (Text.pack . show $ ipk) `Text.append` " on " `Text.append` turi
display (URL turi) = "Site " `Text.append` turi
display (URI turi) = turi

make :: Text -> Identity
make str =
  case result of
    Left _ -> URI str
    Right r -> r
  where
    result = parseOnly identityParser str

identityParser :: Parser Identity
identityParser = try identParser <|> try pkParser <|> urlParser

identParser :: Parser Identity
identParser = do
  _ <- string "sgn://"
  turi <- takeWhile $ inClass "a-zA-Z."
  _ <- string "/?ident="
  tname <- takeWhile $ inClass "a-zA-Z0-9_-"
  endOfInput
  return $ Ident turi tname

pkParser :: Parser Identity
pkParser = do
  _ <- string "sgn://"
  turi <- takeWhile $ inClass "a-zA-Z."
  _ <- string "/?pk="
  ipk <- decimal
  endOfInput
  return $ PK turi ipk

urlParser :: Parser Identity
urlParser = do
  _ <- string "http://" <|> string "https://"
  turl <- takeWhile $ const True
  endOfInput
  return $ URL turl

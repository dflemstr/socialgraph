{-# LANGUAGE OverloadedStrings #-}
module Network.JSON
       ( fetch
       , parse
       , fetchAndParse
       ) where

import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Parser as Parser
import qualified Data.Attoparsec.Lazy as Attoparsec
import Network.BufferType (BufferType)
import qualified Network.HTTP as HTTP
import qualified Network.HTTP.Proxy as Proxy
import qualified Network.Browser as Browser
import qualified Network.URI as URI

fetch :: Text -> IO (Either Text Aeson.Value)
fetch url = do
  proxy <- Proxy.fetchProxy False
  (_, response) <- Browser.browse $ do
    Browser.setOutHandler $ const $ return ()
    Browser.setProxy proxy
    Browser.request $ getRequest url
  contents <- readResponse response  
  return . mapLeft Text.pack . parseJSON $ contents
  where
    readResponse = HTTP.getResponseBody . Right
    parseJSON = Attoparsec.eitherResult . Attoparsec.parse Parser.json

getRequest :: BufferType b => Text -> HTTP.Request b
getRequest urlString =
  case URI.parseURI . Text.unpack $ urlString of
    Nothing -> error $ "getRequest: Not a valid URL - " ++ Text.unpack urlString
    Just u  -> HTTP.mkRequest HTTP.GET u

parse :: Aeson.FromJSON a => Aeson.Value -> Either Text a
parse = resultToEither . Aeson.fromJSON

fetchAndParse :: Aeson.FromJSON a => Text -> IO (Either Text a)
fetchAndParse url = do
  json <- fetch url
  case json of
    Right r -> return $ parse r
    Left l -> return $ Left l

mapLeft :: (a -> c) -> Either a b -> Either c b
mapLeft f (Left a) = Left $ f a
mapLeft _ (Right b) = Right b

resultToEither :: Aeson.Result a -> Either Text a
resultToEither (Aeson.Error s) = Left . Text.pack $ s
resultToEither (Aeson.Success a) = Right a

{-# LANGUAGE OverloadedStrings, DeriveDataTypeable #-}
module Main (main) where

import Prelude hiding (log)
import Data.Data
import Control.Monad.State
import qualified Data.Gephi as Gephi
import Data.SocialGraph.Graph (Graph)
import qualified Data.SocialGraph.Graph as Graph
import qualified Data.SocialGraph.Node as Node
import qualified Network.JSON as JSON
import qualified Network.SocialGraph.QueryResult as QueryResult
import qualified Text.XML.Light as XML
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.HashSet as Set
import qualified Data.List as List
import Data.StringCache (StringCache)
import qualified Data.StringCache as StringCache
import qualified Data.SocialGraph.Convert as Convert
import System.Console.CmdArgs.Implicit
import System.IO as IO
import System.Environment as Environment

data Configuration =
  Configuration { file :: String
                , output :: String
                , outgoing :: Bool
                , incoming :: Bool
                , allIdentities :: Bool
                , onlyIdentities :: Bool
                , recurseLevels :: Int
                , uris :: [String]
                }
  deriving (Show, Data, Typeable)

main :: IO ()
main = do
  pname <- Environment.getProgName
  config <- cmdArgs_ $ configuration pname :: IO Configuration
  runWithConfig config

runWithConfig :: Configuration -> IO ()
runWithConfig config = do
  allURIs <- getURIs config
  log "Fetching initial graph data..."
  let mkURL = queryURL config
      levels = recurseLevels config
      getGephi = Convert.graphToGexf =<< getFullGraph mkURL allURIs levels
  gephi <- evalStateT getGephi StringCache.empty
  log "Writing output..."
  let xml = Gephi.xmlGexf gephi
      write = if null $ output config then putStrLn else writeFile $ output config
  write $ XML.ppTopElement xml
  log "Done."

getFullGraph :: (Text -> Text) -> [Text] -> Int -> StateT StringCache IO Graph
getFullGraph mkURL = go Graph.empty
    where
      go graph _    r | r < 0 = return graph
      go graph urls r = do
        lift $ log $ "Fetching identities (" ++ show r ++ " recursions left)"
        newGraph <- fetchGraphs mkURL urls
        let !combinedGraph = newGraph `Graph.merge` graph
        newUrls <- mapM (StringCache.getString . Node.key) ((Set.toList . Graph.nodes) newGraph)
        go combinedGraph newUrls $ r - 1

getURIs :: Configuration -> IO [Text]
getURIs config = do
  fileURIs <- readURIs $ file config
  return . map Text.pack $ fileURIs ++ uris config

readURIs :: FilePath -> IO [String]
readURIs "" = return []
readURIs ufile = fmap lines $ readFile ufile

fetchGraphs :: (Text -> Text) -> [Text] -> StateT StringCache IO Graph
fetchGraphs mkQueryURI urls = do
  graphs <- mapM fetchGraphFromParam params
  return $ List.foldl' (flip Graph.merge) Graph.empty graphs
  where
    urlCount = length urls
    fetchGraphFromParam = uncurry3 $ fetchGraph urlCount 3
    params = zip3 queryURIs urls [1..]
    queryURIs = map mkQueryURI urls

fetchGraph :: Int -> Int -> Text -> Text -> Int -> StateT StringCache IO Graph
fetchGraph count numRetries url displayName index = do
  lift $ log $ " (" ++ show index ++ "/" ++ show count ++ ") Fetching " ++ Text.unpack displayName
  result <- lift $ JSON.fetchAndParse url
  case result of
    Left err ->
      if numRetries <= 0
      then error $ Text.unpack err
      else do
        let dname = Text.unpack displayName
        lift $ log $ " Error while fetching " ++ dname ++ ": " ++ Text.unpack err
        lift $ log $ " -> Retrying " ++ dname ++ " (" ++ show (numRetries - 1) ++ " retries left)"
        fetchGraph count (numRetries - 1) url displayName index
    Right r -> QueryResult.toGraph r

putErrLn :: String -> IO ()
putErrLn = IO.hPutStr IO.stderr . (++ "\n")

log :: String -> IO ()
log = whenLoud . putErrLn

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f1 (a, b, c) = f1 a b c

queryURL :: Configuration -> Text -> Text
queryURL config uri =
  "http://socialgraph.googleapis.com/lookup?q=" ++: uri ++: endStuff
  where
    (++:) = Text.append
    endStuff = (if outgoing config then "&edo=1" else "") ++:
               (if incoming config then "&edi=1" else "") ++:
               (if allIdentities config then "&fme=1" else "") ++:
               (if onlyIdentities config then "&jme=1" else "") ++: "&sgn=1"

typBoolean :: Ann
typBoolean = typ "true|false"

configuration :: String -> Annotate Ann
configuration pname =
  record (Configuration "" "" False False False False 0 []) [
    file :=
      def
      += help "A file containing a list of identity URLs"
      += typFile
      += name "f"
    , output :=
      def
      += help "The path to the resulting GEXF file"
      += typFile
      += name "o"
    , outgoing :=
      def
      += explicit
      += help "Also graph identities the specified identities have outgoing connections to. For a Twitter account, this will load the identities that account is following."
      += typBoolean
      += name "O"
      += name "outgoing"
    , incoming :=
      def
      += explicit
      += help "Also graph identities that have connections to the specified identities. For a Twitter account, this will load the accounts followers"
      += typBoolean
      += name "I"
      += name "incoming"
    , allIdentities :=
      def
      += explicit
      += help "For each identity, load associated \"me\" identities, and then load associated nodes. If you only specify a persons Twitter account, this will load the E-Mail and blog accounts as well before continuing the query"
      += typBoolean
      += name "a"
      += name "all"
    , onlyIdentities :=
      def
      += explicit
      += help "Only load connected \"me\" identities, ignoring any other type of connection"
      += typBoolean
      += name "m"
      += name "me"
    , recurseLevels :=
      def
      += explicit
      += help "The number of levels to recursively fetch data. Values above 1 are not recommended."
      += name "r"
      += name "recurse"
    , uris :=
      def
      += args
      += typ "URI"
    ]
  += program pname
  += summary (pname ++ " queries the Google SocialGraph API and produces Gephi graphs out of the fetched data")
  += verbosity

module Echidna.MCP (runMCPServer) where

import Control.Monad.IO.Class (liftIO)
import Data.Aeson (encode)
import Data.IORef (readIORef)
import Data.List (find, isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Set (toList)
import Data.Text (pack, unpack)
import Data.Text.Lazy (toStrict)
import Data.Text.Lazy.Encoding (decodeUtf8)
import MCP.Server
import Network.Wai.Handler.Warp (Port)

import Echidna.Types.Config (Env(..))
import Echidna.Types.Corpus (corpusSize)
import Echidna.Types.Tx (Tx)
import Echidna.Pretty (ppTx)

getCorpusSize :: Env -> ToolCallHandler IO
getCorpusSize env _ _ = do
  corpus <- liftIO $ readIORef env.corpusRef
  pure $ Right $ ContentText $ pack $ show $ corpusSize corpus

inspectCorpusTransactions :: Env -> ToolCallHandler IO
inspectCorpusTransactions env _ args = do
  corpus <- liftIO $ readIORef env.corpusRef
  let
    sequence' = read $ unpack $ fromMaybe "0" $ lookup "sequence" args
    page = read $ unpack $ fromMaybe "0" $ lookup "page" args
    txs = fromMaybe [] $ snd <$> find (\(i, _) -> i == sequence') (toList corpus)
    paginatedTxs = take 10 $ drop (page * 10) txs
  pure $ Right $ ContentText $ pack $ unlines $ map (ppTx mempty) paginatedTxs

findTransactionInCorpus :: Env -> ToolCallHandler IO
findTransactionInCorpus env _ args = do
  corpus <- liftIO $ readIORef env.corpusRef
  let
    query = unpack $ fromMaybe "" $ lookup "query" args
    results =
      [ (seq', i `div` 10)
      | (seq', txs) <- toList corpus
      , (tx, i) <- zip txs [0..]
      , query `isInfixOf` ppTx mempty tx
      ]
  pure $ Right $ ContentText $ pack $ show results

runMCPServer :: Env -> Port -> IO ()
runMCPServer env port = do
  let
    info = McpServerInfo "Echidna" "2.2.7" "Echidna MCP server"
    tools' =
      [ ToolDefinition "getCorpusSize" "Get the current corpus size" (InputSchemaDefinitionObject [] []) Nothing
      , ToolDefinition "inspectCorpusTransactions" "Inspect corpus transactions"
          (InputSchemaDefinitionObject
            [ ("sequence", InputSchemaDefinitionProperty "integer" "Sequence number")
            , ("page", InputSchemaDefinitionProperty "integer" "Page number (10 txs per page)")
            ]
            ["sequence", "page"])
          Nothing
      , ToolDefinition "findTransactionInCorpus" "Find transaction in corpus"
          (InputSchemaDefinitionObject
            [ ("query", InputSchemaDefinitionProperty "string" "String to search")
            ]
            ["query"])
          Nothing
      ]
    toolHandler :: ToolCallHandler IO
    toolHandler "getCorpusSize" args = getCorpusSize env "getCorpusSize" args
    toolHandler "inspectCorpusTransactions" args = inspectCorpusTransactions env "inspectCorpusTransactions" args
    toolHandler "findTransactionInCorpus" args = findTransactionInCorpus env "findTransactionInCorpus" args
    toolHandler _ _ = pure $ Left $ InternalError "Unknown tool"
    handlers = McpServerHandlers
      { prompts = Nothing
      , resources = Nothing
      , tools = Just (pure tools', toolHandler)
      }
  runMcpServerHttpWithConfig (HttpConfig {httpPort = port, httpHost = "127.0.0.1", httpEndpoint = "/mcp", httpVerbose = False}) info handlers
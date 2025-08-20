module Echidna.MCP (runMCPServer) where

import Control.Monad.IO.Class (liftIO)
import Data.IORef (readIORef)
import Data.Text (pack)
import MCP.Server
import Network.Wai.Handler.Warp (Port)

import Echidna.Types.Config (Env(..))
import Echidna.Types.Corpus (corpusSize)

getCorpusSize :: Env -> ToolCallHandler IO
getCorpusSize env _ _ = do
  corpus <- liftIO $ readIORef env.corpusRef
  pure $ Right $ ContentText $ pack $ show $ corpusSize corpus

runMCPServer :: Env -> Port -> IO ()
runMCPServer env port = do
  let
    info = McpServerInfo "Echidna" "2.2.7" "Echidna MCP server"
    tools =
      [ ToolDefinition "getCorpusSize" "Get the current corpus size" (InputSchemaDefinitionObject [] []) Nothing
      ]
    handlers = McpServerHandlers
      { prompts = Nothing
      , resources = Nothing
      , tools = Just (pure tools, getCorpusSize env)
      }
  runMcpServerHttpWithConfig (HttpConfig {httpPort = port, httpHost = "127.0.0.1", httpEndpoint = "/mcp", httpVerbose = False}) info handlers
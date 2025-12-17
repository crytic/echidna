{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Echidna.MCP where

import Control.Concurrent.STM
import Data.IORef (readIORef, IORef)
import Data.List (find)
import qualified Data.Set as Set
import Data.Text (Text, pack, unpack)
import qualified Data.Text as T
import Text.Printf (printf)

import MCP.Server

import Echidna.Types.Config (Env(..), EConfig(..))
import Echidna.Types.Campaign (getNFuzzWorkers)
import Echidna.Types.InterWorker (Bus, Message(..), WrappedMessage(..), AgentId(..), BroadcastMsg(..), FuzzerCmd(..))

-- | MCP Tool Definition
-- Simulates the definition of a tool exposed by an MCP server.
data Tool = Tool
  { toolName :: String
  , toolDescription :: String
  , execute :: [(Text, Text)] -> Env -> Bus -> IORef [Text] -> IO String
  }

-- | Registry of available tools
availableTools :: [Tool]
availableTools =
  [ Tool "read_corpus" "Read the current corpus size" $ \_ env _ _ -> do
      c <- readIORef env.corpusRef
      return $ printf "Corpus Size: %d" (Set.size c)
  , Tool "broadcast_message" "Broadcast a text message to all agents" $ \args _ bus _ -> do
      -- Extract "message" argument or join all values
      let msg = case lookup "message" args of
                  Just m -> m
                  Nothing -> T.unwords $ map snd args
      atomically $ writeTChan bus (WrappedMessage AIId (Broadcast (StrategyUpdate msg)))
      return $ printf "Broadcasted: %s" (unpack msg)
  , Tool "dump_lcov" "Dump coverage in LCOV format" $ \_ _ bus _ -> do
      atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer 0 DumpLcov))
      return "Requested LCOV dump from Fuzzer 0"
  , Tool "prioritize_function" "Prioritize a function for fuzzing" $ \args env bus _ -> do
      let msg = case lookup "function" args of
                  Just m -> m
                  Nothing -> ""
      let nWorkers = getNFuzzWorkers env.cfg.campaignConf
      mapM_ (\i -> atomically $ writeTChan bus (WrappedMessage AIId (ToFuzzer i (PrioritizeFunction (unpack msg))))) [0 .. nWorkers - 1]
      return $ printf "Requested prioritization of function '%s' on %d fuzzers" (unpack msg) nWorkers
  , Tool "read_logs" "Read the last 100 log messages" $ \_ _ _ logsRef -> do
      logs <- readIORef logsRef
      return $ unpack $ T.unlines $ reverse logs
  ]

-- | Run the MCP Server
runMCPServer :: Env -> Int -> IORef [Text] -> IO ()
runMCPServer env port logsRef = do
    let httpConfig = HttpConfig
            { httpPort = port
            , httpHost = "127.0.0.1"
            , httpEndpoint = "/mcp"
            , httpVerbose = False
            }

    let serverInfo = McpServerInfo
            { serverName = "Echidna MCP Server"
            , serverVersion = "1.0.0"
            , serverInstructions = "Echidna Agent Interface. Available tools: read_corpus, broadcast_message, dump_lcov"
            }

    let mkToolDefinition :: Tool -> ToolDefinition
        mkToolDefinition t = ToolDefinition
            { toolDefinitionName = pack t.toolName
            , toolDefinitionDescription = pack t.toolDescription
            , toolDefinitionInputSchema = case t.toolName of
                "broadcast_message" -> InputSchemaDefinitionObject
                    { properties = [("message", InputSchemaDefinitionProperty "string" "The message to broadcast")]
                    , required = ["message"]
                    }
                "dump_lcov" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                "prioritize_function" -> InputSchemaDefinitionObject
                    { properties = [("function", InputSchemaDefinitionProperty "string" "The name of the function to prioritize")]
                    , required = ["function"]
                    }
                "read_logs" -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
                _ -> InputSchemaDefinitionObject
                    { properties = []
                    , required = []
                    }
            , toolDefinitionTitle = Nothing
            , toolDefinitionMeta = Nothing
            }

    let toolDefs = map mkToolDefinition availableTools

    let handleToolCall :: ToolName -> [(ArgumentName, ArgumentValue)] -> IO (Either Error Content)
        handleToolCall name args = do
            case find (\t -> pack (t.toolName) == name) availableTools of
                Nothing -> return $ Left $ UnknownTool name
                Just tool -> do
                    result <- tool.execute args env env.bus logsRef
                    return $ Right $ ContentText $ pack result

    let handlers = McpServerHandlers
            { prompts = Nothing
            , resources = Nothing
            , tools = Just (return toolDefs, handleToolCall)
            }

    runMcpServerHttpWithConfig httpConfig serverInfo handlers

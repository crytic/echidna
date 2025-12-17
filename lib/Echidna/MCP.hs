{-# LANGUAGE OverloadedStrings #-}
module Echidna.MCP where

import Control.Concurrent.STM
import Data.IORef (readIORef)
import qualified Data.Set as Set
import Data.Text (pack)
import Text.Printf (printf)

import Echidna.Types.Config (Env(..))
import Echidna.Types.InterWorker (Bus, Message(..), WrappedMessage(..), AgentId(..), BroadcastMsg(..))

-- | MCP Tool Definition
-- Simulates the definition of a tool exposed by an MCP server.
data Tool = Tool
  { toolName :: String
  , toolDescription :: String
  , execute :: [String] -> Env -> Bus -> IO String
  }

-- | Registry of available tools
availableTools :: [Tool]
availableTools =
  [ Tool "read_corpus" "Read the current corpus size" $ \_ env _ -> do
      c <- readIORef env.corpusRef
      return $ printf "Corpus Size: %d" (Set.size c)
  , Tool "broadcast_message" "Broadcast a text message to all agents" $ \args _ bus -> do
      let msg = unwords args
      atomically $ writeTChan bus (WrappedMessage AIId (Broadcast (StrategyUpdate (pack msg))))
      return $ printf "Broadcasted: %s" msg
  ]

-- | Execute an MCP Tool
executeTool :: String -> [String] -> Env -> Bus -> IO String
executeTool name args env bus = do
  let matches = filter (\t -> t.toolName == name) availableTools
  case matches of
    [] -> return $ printf "Error: Tool '%s' not found." name
    (t:_) -> t.execute args env bus

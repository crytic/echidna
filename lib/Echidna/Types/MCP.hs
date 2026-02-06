{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}

module Echidna.Types.MCP where

import Data.Aeson (Value, ToJSON(..), FromJSON(..), object, (.=))
import Data.IORef (IORef)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Text (Text)
import Data.Time.LocalTime (LocalTime)
import GHC.Generics (Generic)

-- | MCP Tool Definition
-- Represents a single MCP tool exposed to AI agents
data MCPTool = MCPTool
  { toolName :: !Text
    -- ^ Tool identifier (e.g., "read_logs", "inject_transaction")
  , toolDescription :: !Text
    -- ^ Human-readable description for agent consumption
  , toolInputSchema :: !Value
    -- ^ JSON Schema defining expected parameters (draft-07)
  , toolHandler :: Value -> IO ToolResult
    -- ^ Execution function taking JSON input
  }

-- | Result of tool execution
data ToolResult = ToolResult
  { success :: !Bool
    -- ^ Whether the tool execution succeeded
  , content :: !Value
    -- ^ JSON response for agent (observability data or confirmation message)
  , errorMsg :: !(Maybe Text)
    -- ^ Error description if success is False
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Event Log Entry
-- Single event in the ring buffer for agent observation
data EventEntry = EventEntry
  { entryTimestamp :: !LocalTime
    -- ^ Event occurrence time (wall-clock, not simulation time)
  , entryType :: !Text
    -- ^ Event category (e.g., "PropertyFalsified", "TransactionExecuted", "CoverageIncreased")
  , entryWorkerId :: !Int
    -- ^ Worker that generated event (0-based)
  , entryData :: !Value
    -- ^ Event-specific JSON payload
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Event Log Ring Buffer
-- Circular buffer of campaign events for agent observation (FR-001, FR-002)
data EventLog = EventLog
  { eventEntries :: !(IORef (Seq EventEntry))
    -- ^ Circular buffer of max 2500 entries (Clarification #4)
  , eventCapacity :: !Int
    -- ^ Fixed at 2500
  , eventNextIndex :: !(IORef Int)
    -- ^ Insertion pointer for circular buffer
  }

-- | MCP Command Log Entry
-- Audit log entry for control commands (reproducibility, Principle III)
data MCPCommandLog = MCPCommandLog
  { commandTimestamp :: !LocalTime
    -- ^ Command execution time (wall-clock)
  , commandToolName :: !Text
    -- ^ Executed tool (e.g., "inject_transaction")
  , commandParameters :: !(Map Text Value)
    -- ^ Input parameters as key-value JSON
  , commandResult :: !CommandResult
    -- ^ Success or error outcome
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Command execution result
data CommandResult = CommandResult
  { resultSuccess :: !Bool
    -- ^ Whether command succeeded
  , resultMessage :: !Text
    -- ^ Human-readable result or error description
  } deriving (Show, Generic, ToJSON, FromJSON)

-- | Tool Execution Context
-- Runtime information passed to tool handlers
data ToolExecution = ToolExecution
  { toolInput :: !Value
    -- ^ JSON input from agent (validated against inputSchema)
  , executionTimestamp :: !LocalTime
    -- ^ When the tool was invoked
  }

# Data Model: MCP Agent Commands

**Feature**: 001-mcp-agent-commands  
**Phase**: 1 - Design  
**Date**: 2025-12-20

---

## Overview

This document defines the core data structures for MCP integration in Echidna. Entities are extracted from [spec.md](spec.md) functional requirements and refined based on [research.md](research.md) findings.

---

## Core Entities

### 1. MCPTool

**Purpose**: Represents a single MCP tool exposed to agents.

**Fields**:
- `name :: Text` - Tool identifier (e.g., "read_logs", "inject_transaction")
- `description :: Text` - Human-readable description for agent consumption
- `inputSchema :: Value` - JSON Schema defining expected parameters
- `handler :: Value -> IO ToolResult` - Execution function

**Validation Rules**:
- Name must be unique within tool registry
- Name must match regex `^[a-z_]+$` (lowercase, underscores only)
- InputSchema must be valid JSON Schema draft-07

**Relationships**:
- **MCPTool → ToolExecution** (1:N) - A tool can be executed multiple times

**State Transitions**: Immutable once registered

**Haskell Definition**:
```haskell
-- lib/Echidna/Types/MCP.hs
data MCPTool = MCPTool
  { toolName :: !Text
  , toolDescription :: !Text
  , toolInputSchema :: !Value
  , toolHandler :: Value -> IO ToolResult
  }

data ToolResult = ToolResult
  { success :: !Bool
  , content :: !Value -- JSON response for agent
  , errorMsg :: !(Maybe Text)
  }
```

---

### 2. EventLog

**Purpose**: Ring buffer of campaign events for agent observation (FR-001, FR-002).

**Fields**:
- `entries :: IORef (Seq EventEntry)` - Circular buffer (max 2500 entries, Clarification #4)
- `capacity :: Int` - Fixed at 2500
- `nextIndex :: IORef Int` - Insertion pointer for circular buffer

**EventEntry Structure**:
- `timestamp :: LocalTime` - Event occurrence time
- `eventType :: Text` - Event category (e.g., "PropertyFalsified", "TransactionExecuted", "CoverageIncreased")
- `workerId :: Int` - Worker that generated event (0-based)
- `data :: Value` - Event-specific JSON payload

**Validation Rules**:
- Capacity must be 2500 (Clarification #4)
- Timestamps must be monotonically increasing per worker
- Data field must be valid JSON

**Behavior**:
- When full, overwrites oldest entry (drop-oldest ring buffer)
- Thread-safe via IORef atomicModifyIORef'
- Read-only from MCP tools (no external mutations)

**Relationships**:
- **EventLog → ToolExecution** (1:N) - Tools query EventLog for observations

**State Transitions**:
```
Empty → Filling → Full (circular)
      ↓            ↓
   [0..2499]  Overwrite oldest
```

**Haskell Definition**:
```haskell
-- lib/Echidna/Types/MCP.hs
data EventLog = EventLog
  { eventEntries :: !(IORef (Seq EventEntry))
  , eventCapacity :: !Int -- Always 2500
  , eventNextIndex :: !(IORef Int)
  }

data EventEntry = EventEntry
  { entryTimestamp :: !LocalTime
  , entryType :: !Text
  , entryWorkerId :: !Int
  , entryData :: !Value
  } deriving (Show, Generic, ToJSON, FromJSON)
```

---

### 3. MCPCommandLog

**Purpose**: Audit log of control commands for reproducibility (Principle III, FR-010).

**Fields**:
- `timestamp :: LocalTime` - Command execution time
- `toolName :: Text` - Executed tool (e.g., "inject_transaction")
- `parameters :: Map Text Value` - Input parameters as key-value JSON
- `result :: CommandResult` - Success or error outcome

**CommandResult Structure**:
- `success :: Bool` - Whether command succeeded
- `message :: Text` - Human-readable result or error description

**Validation Rules**:
- Timestamp must be wall-clock time (not simulation time)
- ToolName must match one of the 3 control tools: inject_transaction, prioritize_function, clear_priorities
- Parameters must serialize to valid JSON

**Persistence**:
- File path: `{corpus-dir}/mcp-commands.jsonl` (Clarification #6)
- Format: JSONL (JSON Lines) - one entry per line
- Example entry:
  ```json
  {"timestamp":"2025-12-20T10:30:45Z","tool":"inject_transaction","args":{"sequence":["transfer(0x123,100)"],"solc_version":"0.8.0"},"result":"success"}
  ```

**Relationships**:
- **MCPCommandLog → Bus** (1:1) - Commands trigger Bus messages
- **MCPCommandLog ← ToolExecution** (1:1) - Each control tool execution creates one log entry

**State Transitions**:
```
Pending → Executed → Logged
        ↓           ↓
  Write to JSONL  Flush to disk
```

**Haskell Definition**:
```haskell
-- lib/Echidna/Types/MCP.hs
data MCPCommandLog = MCPCommandLog
  { cmdTimestamp :: !LocalTime
  , cmdTool :: !Text
  , cmdParameters :: !(Map Text Value)
  , cmdResult :: !CommandResult
  } deriving (Show, Generic, ToJSON, FromJSON)

data CommandResult = CommandResult
  { cmdSuccess :: !Bool
  , cmdMessage :: !Text
  } deriving (Show, Generic, ToJSON, FromJSON)
```

---

### 4. Bus (Extension)

**Purpose**: Message queue for inter-worker communication (existing type, extended).

**Existing Definition** (from research.md):
```haskell
-- lib/Echidna/Types/InterWorker.hs
type Bus = TChan WrappedMessage

data WrappedMessage = WrappedMessage
  { sender :: AgentId
  , recipient :: AgentId -- or Broadcast
  , payload :: FuzzerCmd
  }

data FuzzerCmd
  = Pause | Resume | Shutdown
  | SharedCorpus [Tx] -- Existing
  -- NEW: Control commands
  | InjectTransaction [Tx] -- Clarification #5: Broadcast to all workers
  | LogMCPCommand MCPCommandLog
```

**New Fields** (none - Bus is TChan, no fields change):
- Bus remains `TChan WrappedMessage`

**New Message Types** (extending FuzzerCmd):
1. **InjectTransaction [Tx]**:
   - Purpose: External transaction injection (FR-005)
   - Validation: Tx list non-empty, valid Solidity syntax (Clarification #1)
   - Behavior: Broadcast to all workers (Clarification #5)

2. **LogMCPCommand MCPCommandLog**:
   - Purpose: Notify logger to persist command
   - Validation: MCPCommandLog well-formed
   - Behavior: Unicast to logging worker

**Relationships**:
- **Bus ← MCPTool** (N:1) - Control tools write to Bus
- **Bus → Worker** (1:N) - Workers read from Bus

**State Transitions**:
```
Message Created → Enqueued → Dequeued → Processed
              (atomically)  (atomically)
```

**Validation Rules**:
- InjectTransaction: All Tx must parse as valid Solidity function calls
- LogMCPCommand: Must reference executed control tool

**Haskell Extension**:
```haskell
-- lib/Echidna/Types/InterWorker.hs (EXTEND existing)
data FuzzerCmd
  = Pause | Resume | Shutdown
  | SharedCorpus [Tx]
  | InjectTransaction [Tx] -- NEW
  | LogMCPCommand MCPCommandLog -- NEW
  deriving (Show, Eq, Generic)
```

---

### 5. ToolExecution

**Purpose**: Tracks execution metadata for performance monitoring and debugging.

**Fields**:
- `execId :: UUID` - Unique execution identifier
- `toolName :: Text` - Executed tool
- `startTime :: LocalTime` - Execution start timestamp
- `endTime :: LocalTime` - Execution completion timestamp
- `duration :: NominalDiffTime` - Computed as `endTime - startTime`
- `success :: Bool` - Whether tool returned success result
- `errorMsg :: Maybe Text` - Error description if failed

**Validation Rules**:
- Duration must be < 100ms (SC-008: response time target)
- ToolName must exist in tool registry
- EndTime must be >= StartTime

**Relationships**:
- **ToolExecution → MCPTool** (N:1) - Execution references tool definition
- **ToolExecution → EventLog** (N:1, optional) - Observability tools query EventLog
- **ToolExecution → MCPCommandLog** (1:1, optional) - Control tools create log entries

**State Transitions**:
```
Pending → Running → Completed
        ↓          ↓
   startTime   endTime, success
```

**Usage**:
- In-memory only (not persisted)
- Used for performance metrics (SC-008)
- UI can display recent executions for debugging

**Haskell Definition**:
```haskell
-- lib/Echidna/Types/MCP.hs
data ToolExecution = ToolExecution
  { execId :: !UUID
  , execTool :: !Text
  , execStart :: !LocalTime
  , execEnd :: !LocalTime
  , execDuration :: !NominalDiffTime
  , execSuccess :: !Bool
  , execError :: !(Maybe Text)
  } deriving (Show, Generic)
```

---

### 6. PrioritizedFunctions (Extension to GenDict)

**Purpose**: Stores function prioritization state for guided fuzzing (FR-006, FR-007).

**Existing Context** (from research.md):
```haskell
-- lib/Echidna/Types/Campaign.hs
data GenDict = GenDict
  { dictValues :: HashMap AbiValue Int
  , dictConsts :: HashMap W256 Int
  -- NEW:
  , prioritizedFunctions :: IORef (Set Text) -- Function signatures
  , priorityWeight :: Double -- 0.9 = 90% prioritized, 10% random (Clarification #2)
  }
```

**New Fields**:
1. `prioritizedFunctions :: IORef (Set Text)`:
   - Stores function signatures to prioritize (e.g., "transfer(address,uint256)")
   - IORef for thread-safe updates
   - Set ensures no duplicates

2. `priorityWeight :: Double`:
   - Fixed at 0.9 (90% prioritized / 10% random, Clarification #2)
   - Used by fuzzer's function selection logic

**Validation Rules**:
- Function signatures must be canonical ABI format (no spaces, type keywords)
- priorityWeight must be 0.9 (enforced at initialization)
- prioritizedFunctions can be empty (all functions equally weighted)

**Behavior**:
- Fuzzer checks prioritizedFunctions before selecting next function
- 90% of time: sample from prioritizedFunctions (uniform within set)
- 10% of time: sample from all functions (fallback to default behavior)
- `clear_priorities` resets set to empty

**Relationships**:
- **GenDict ← Bus** (1:N) - PrioritizeFunction and ClearPriorities messages update GenDict
- **GenDict → Fuzzer** (1:N) - Each worker reads GenDict for function selection

**State Transitions**:
```
Empty → Partially Prioritized → Cleared → ...
      ↓                       ↓
  (prioritize_function)   (clear_priorities)
```

**Haskell Extension**:
```haskell
-- lib/Echidna/Types/Campaign.hs (EXTEND existing)
data GenDict = GenDict
  { dictValues :: HashMap AbiValue Int
  , dictConsts :: HashMap W256 Int
  , prioritizedFunctions :: !(IORef (Set Text)) -- NEW
  , priorityWeight :: !Double -- NEW: Always 0.9
  }
```

---

### 7. Env (Extension)

**Purpose**: Global environment state (existing type, extended with MCP fields).

**Existing Definition** (from research.md):
```haskell
-- lib/Echidna/Types/Config.hs
data Env = Env
  { cfg :: Config
  , dapp :: DApp
  , codehashMap :: CodehashMap
  , coverageRef :: IORef CoverageMap
  , corpusRef :: IORef [Tx]
  , testRefs :: IORef [TestState]
  , gasInfoRef :: IORef [GasInfo]
  , fetchContractCache :: FetchContractCache
  , fetchSlotCache :: FetchSlotCache
  , eventQueue :: Chan (LocalTime, CampaignEvent)
  , bus :: Bus
  -- NEW:
  , eventLog :: EventLog -- Ring buffer for MCP observations
  , mcpCommandLog :: IORef [MCPCommandLog] -- In-memory before flush
  , mcpServer :: Maybe ThreadId -- MCP server thread
  }
```

**New Fields**:
1. `eventLog :: EventLog`:
   - Ring buffer (2500 entries) for campaign events
   - Populated by existing `eventQueue` consumer
   - Read by observability tools (read_logs, show_coverage, etc.)

2. `mcpCommandLog :: IORef [MCPCommandLog]`:
   - In-memory accumulator before JSONL flush
   - Batched writes to `{corpus-dir}/mcp-commands.jsonl`
   - Flushed periodically or at campaign end

3. `mcpServer :: Maybe ThreadId`:
   - ThreadId of MCP server (if started via `--mcp-port` flag)
   - Used for graceful shutdown
   - Nothing if MCP not enabled

**Validation Rules**:
- eventLog capacity fixed at 2500
- mcpCommandLog flushed before process exit
- mcpServer thread must be joinable on shutdown

**Relationships**:
- **Env → EventLog** (1:1) - Single global EventLog
- **Env → Bus** (1:1) - Single global Bus
- **Env → MCPServer** (1:1) - Single MCP server thread

**Initialization**:
```haskell
-- lib/Echidna/Types/Config.hs extension
initEnv :: Config -> DApp -> IO Env
initEnv cfg dapp = do
  -- ... existing initialization
  eventLog <- newEventLog 2500
  mcpCommandLog <- newIORef []
  mcpServer <- if cfg.uiConf.mcpPort > 0
               then Just <$> spawnMCPServer cfg.uiConf.mcpPort
               else pure Nothing
  pure Env { ..., eventLog, mcpCommandLog, mcpServer }
```

---

## Entity Relationships Diagram

```
┌─────────────┐
│   MCPTool   │──────┐
└─────────────┘      │
                     │ 1:N
                     ▼
              ┌──────────────┐      ┌──────────────┐
              │ToolExecution │◀────▶│  EventLog    │
              └──────────────┘ N:1  └──────────────┘
                     │                      ▲
                     │ 1:1 (control)        │ 1:1
                     ▼                      │
              ┌──────────────┐              │
              │MCPCommandLog │              │
              └──────────────┘              │
                     │                      │
                     │ 1:N                  │
                     ▼                      │
              ┌──────────────┐        ┌─────────┐
              │     Bus      │◀──────▶│   Env   │
              └──────────────┘   1:1  └─────────┘
                     │                      │
                     │ 1:N                  │ 1:1
                     ▼                      ▼
              ┌──────────────┐    ┌─────────────────┐
              │   Workers    │    │PrioritizedFuncs │
              │  (Fuzzer)    │    │   (GenDict)     │
              └──────────────┘    └─────────────────┘
```

---

## Data Flow

### Observability Path (read_logs, show_coverage, etc.)
```
Campaign Event → eventQueue → EventLog (ring buffer) → MCPTool.handler → Agent
```

### Control Path (inject_transaction, prioritize_function, clear_priorities)
```
Agent → MCP HTTP Request → MCPTool.handler → Bus (InjectTransaction/...) → Workers → MCPCommandLog → JSONL file
```

### Monitoring Path (inspect_corpus_transactions, find_transaction_in_corpus)
```
Agent → MCP HTTP Request → MCPTool.handler → Read corpusRef → JSON Response → Agent
```

---

## Implementation Notes

### Thread Safety
- **EventLog**: IORef with atomicModifyIORef' for ring buffer updates
- **Bus**: TChan provides STM-based concurrency (already thread-safe)
- **MCPCommandLog**: IORef accumulator with periodic batched writes
- **PrioritizedFunctions**: IORef (Set Text) with atomic Set operations

### Performance Considerations
- EventLog ring buffer: O(1) insertion, O(n) read (n ≤ 2500)
- Bus TChan: Non-blocking writes, blocking reads with timeout
- PrioritizedFunctions: O(log n) Set insertion/deletion, O(1) membership check
- MCPCommandLog: Batched JSONL writes to avoid I/O overhead per command

### Serialization
- All entities except Bus/EventLog handlers must implement `ToJSON`/`FromJSON`
- MCPCommandLog persisted as JSONL (one JSON object per line)
- EventEntry exposed as JSON via `read_logs` tool

---

## Validation Checklist

- [x] All entities from spec.md key entities section covered
- [x] Validation rules specified for each entity
- [x] Relationships defined with cardinality
- [x] State transitions documented where applicable
- [x] Haskell type definitions aligned with research.md findings
- [x] Thread-safety mechanisms specified
- [x] Performance characteristics documented
- [x] Serialization strategy defined
- [x] Clarifications #1-6 incorporated (transaction format, prioritization, EventLog size, worker broadcast, log persistence)

---

**Status**: Phase 1 Complete - Data Model Approved  
**Next Phase**: Contracts Generation (API Schemas)  
**Reviewer**: Daniel Tradito  
**Date**: 2025-12-20

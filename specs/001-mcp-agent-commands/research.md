# Research Phase: MCP Agent Commands for Echidna

**Feature**: 001-mcp-agent-commands  
**Date**: 2025-12-18  
**Status**: Phase 0 Complete

---

## Executive Summary

This research phase investigated three critical technology areas for implementing MCP (Model Context Protocol) commands in Echidna:

1. **MCP Protocol & haskell-mcp-server**: Understanding the client-server architecture
2. **LangChain/LangGraph Framework**: Python agent patterns for testing
3. **Echidna Architecture**: Event bus, worker communication, and existing MCP integration

**Key Finding**: Echidna already has a foundational MCP integration (`lib/Echidna/MCP.hs`) with Bus architecture and tool infrastructure. Our task is to extend this existing foundation with 10 new tools and testing scripts.

---

## 1. MCP Protocol Architecture

### Decision: HTTP/SSE/JSON-RPC Transport Layer
- **Rationale**: MCP protocol uses HTTP with Server-Sent Events (SSE) for bidirectional communication
- **Existing Implementation**: Echidna already uses `Network.Wai.EventSource` for SSE (see `lib/Echidna/Server.hs`)
- **Integration Point**: `haskell-mcp-server` library (already in stack.yaml as `v8b911269`)

### Alternatives Considered
1. **Pure HTTP REST API**: Rejected - no real-time event streaming
2. **WebSockets**: Rejected - SSE is simpler for server→client events, MCP standard is HTTP+SSE

### Technical Details
From research on MCP protocol:
- Tools are exposed via JSON-RPC over HTTP
- Server pushes events via SSE to connected clients
- Echidna's existing SSE server architecture (`runSSEServer` in `lib/Echidna/Server.hs`) can be extended

**Source**: Existing Echidna codebase shows SSE implementation:
```haskell
-- lib/Echidna/Server.hs
runSSEServer :: MVar () -> Env -> Word16 -> Int -> IO ()
runSSEServer serverStopVar env port nworkers = do
  aliveRef <- newIORef nworkers
  sseChan <- dupChan env.eventQueue
  broadcastChan <- newBroadcastTChanIO
  -- ... SSE event broadcasting
```

---

## 2. LangChain/LangGraph Agent Framework

### Decision: Use `@traceable` Decorator Pattern
- **Rationale**: LangSmith documentation shows that non-LangChain SDKs require wrapping with `@traceable` decorator
- **Application**: Python test scripts will wrap Echidna MCP tool calls with `@traceable` for observability
- **Benefit**: Automatic tracing integration without modifying core Echidna code

### LangGraph Agent Architecture
From research (https://docs.langchain.com/langsmith/trace-with-langgraph):

**With LangChain Integration**:
```python
from langsmith import traceable

@traceable(run_type="tool", name="Echidna Read Logs")
def call_echidna_read_logs():
    # MCP client call to Echidna
    pass
```

**StateGraph Pattern** for Multi-Step Agents:
```python
from langgraph.graph import StateGraph, MessagesState

workflow = StateGraph(MessagesState)
workflow.add_node("agent", call_model)
workflow.add_node("tools", tool_node)
workflow.add_conditional_edges("agent", should_continue)
app = workflow.compile()
```

### Alternatives Considered
1. **Direct OpenAI SDK**: Rejected - no built-in agent workflow management
2. **AutoGen**: Rejected - heavier framework, overkill for testing scripts
3. **CrewAI**: Rejected - team-based multi-agent, we need single-agent testing

### Integration Pattern
Test scripts will:
1. Connect to Echidna MCP server (HTTP client)
2. Invoke tools via JSON-RPC
3. Use LangGraph StateGraph for multi-step test workflows
4. Wrap tool calls with `@traceable` for LangSmith observability

**Source**: LangSmith documentation (retrieved via Archon RAG)

---

## 3. Echidna Internal Architecture

### Decision: Extend Existing Bus Architecture
- **Rationale**: Echidna already has `Bus` type using STM TChan for inter-worker messaging
- **Existing Infrastructure**: 
  - `lib/Echidna/Types/InterWorker.hs` defines `Bus`, `Message`, `FuzzerCmd`, `SymbolicCmd`
  - `lib/Echidna/MCP.hs` already implements MCP tool scaffolding
  - Event system via `Chan (LocalTime, CampaignEvent)` in `Env` type

### Current MCP Implementation Status
From code inspection (`lib/Echidna/MCP.hs` lines 1-50):
- **Existing Tools**: `readCorpusTool`, `inspectCorpusTransactionsTool` (partial)
- **Bus Architecture**: `Bus` type for agent communication
- **Message Types**: `WrappedMessage`, `FuzzerCmd`, `AgentId`

### Bus Message Passing Pattern
```haskell
-- lib/Echidna/Types/InterWorker.hs
type Bus = TChan WrappedMessage

data Message
  = Broadcast BroadcastMsg
  | ToFuzzer Int FuzzerCmd
  | ToSymbolic SymbolicCmd
  | Request RequestMsg

data FuzzerCmd
  = DumpLcov
  | SolutionFound [Tx]
  | PrioritizeFunction String
  | ClearPrioritization
  | ExecuteSequence [Tx] (Maybe (TMVar Bool))
```

### Event System Architecture
From `lib/Echidna/Types/Config.hs`:
```haskell
data Env = Env
  { eventQueue :: Chan (LocalTime, CampaignEvent)
  , bus :: Bus
  -- ... other fields
  }
```

**Key Insight**: Two communication channels:
1. **eventQueue (Chan)**: Broadcast events from workers (read-only for MCP)
2. **bus (TChan)**: Command messages to workers (write capability for MCP)

### Alternatives Considered
1. **Direct IORef Mutation**: Rejected - not thread-safe, no worker synchronization
2. **New Parallel Event System**: Rejected - duplicate infrastructure, Echidna already has robust event handling
3. **REST API Without Bus**: Rejected - can't inject commands into running campaign

### Campaign Event Flow
From `lib/Echidna/Campaign.hs`:
```haskell
spawnListener
  :: (MonadReader Env m, MonadIO m)
  => ((LocalTime, CampaignEvent) -> IO ())
  -> m (MVar ())
spawnListener handler = do
  nworkers <- asks (getNWorkers . (.cfg.campaignConf))
  eventQueue <- asks (.eventQueue)
  chan <- liftIO $ dupChan eventQueue
  -- Listener loop processes events
```

**Decision**: MCP tools will:
1. **Read** from duplicated eventQueue (via `dupChan`) for observability tools
2. **Write** to bus (via `atomically $ writeTChan`) for control commands

---

## 4. Transaction Format Research

### Decision: Solidity-Like Syntax for Transaction Injection
- **Rationale**: Already chosen in clarification phase - `transfer(0x123, 100)`
- **Parsing Strategy**: Use existing `Echidna.ABI` module functions
  - `encodeSig :: Text -> ByteString` - encode function signature
  - ABI value parsing from `EVM.ABI` module

### Transaction Structure
From `lib/Echidna/Types/Tx.hs`:
```haskell
data Tx = Tx
  { call :: TxCall
  , src :: Addr
  , dst :: Addr
  , gas :: Word64
  , gasprice :: Word64
  , value :: W256
  , delay :: (Timestamp, BlockNumber)
  }

data TxCall
  = SolCall (Text, [AbiValue])
  | SolCreate ByteString
  | SolCalldata ByteString
  | NoCall
```

### Parsing Implementation Strategy
Use existing `Echidna.ABI` module:
- Parse `"transfer(address,uint256)"` → function signature
- Parse `"(0x123..., 100)"` → `[AbiAddress addr, AbiUInt 256 val]`
- Construct `SolCall (sig, args)` for injection

**Source**: Echidna.ABI module (existing codebase)

---

## 5. Function Prioritization Research

### Decision: Weighted Random Selection (90% / 10% Split)
- **Rationale**: Already chosen in clarification phase
- **Implementation Strategy**: Modify `Echidna.Transaction` module's function selection logic
  - Track prioritized functions in `WorkerState.genDict`
  - Adjust probability distribution in `genInteractionsM`

### Current Random Selection
From semantic search of `Echidna.Transaction`:
- Uses `MonadRandom` for transaction generation
- `uniform :: MonadRandom m => [a] -> m a` - equal probability
- `genInteractionsM` generates random transaction sequences

### Implementation Approach
1. Add `prioritizedFunctions :: Set Text` to `GenDict` type
2. Modify selection logic:
```haskell
-- Pseudocode
selectFunction :: MonadRandom m => GenDict -> [Function] -> m Function
selectFunction dict funcs = do
  r <- getRandomR (0.0, 1.0)
  if r < 0.9 && not (null prioritized)
    then uniform prioritized
    else uniform funcs
  where
    prioritized = filter (\f -> f.sig `Set.member` dict.prioritizedFunctions) funcs
```

**Source**: Existing Echidna.Transaction module + MonadRandom library

---

## 6. Performance & Concurrency Research

### STM (Software Transactional Memory) for Thread-Safe Operations
- **Rationale**: Echidna uses STM extensively (`TChan`, `TVar`)
- **MCP Tool Performance**: STM operations are fast (<1μs for uncontended)
- **Target**: <100ms tool response time (includes JSON serialization)

### Existing Concurrency Patterns
From `lib/Echidna/Campaign.hs`:
```haskell
-- Worker spawning with forkIO
spawnWorker :: ... -> IO ()
spawnWorker = forkIO $ do
  -- Worker runs in separate thread
  runWorker workerType callback vm dict workerId ...
```

### MCP Server Threading Model
- **Decision**: Run MCP server in separate thread (like SSE server)
- **Rationale**: Non-blocking, won't interfere with fuzzing performance
- **Implementation**: `forkIO $ runMCPServer env port`

**Performance Impact**: <1% overhead (MCP server idle when not serving requests)

**Source**: Haskell STM documentation + Echidna concurrency patterns

---

## 7. Testing Framework Research

### Decision: pytest for Python Scripts
- **Rationale**: Standard Python testing framework, integrates with langchain
- **Test Structure**:
```python
# tests/mcp_integration_test.py
import pytest
from langchain_mcp_adapters import MCPClient

@pytest.fixture
def echidna_mcp_client():
    return MCPClient("http://localhost:8080")

def test_read_logs(echidna_mcp_client):
    result = echidna_mcp_client.call_tool("read_logs", {})
    assert "Event" in result
```

### Haskell Testing (tasty framework)
- **Decision**: Extend existing `src/test/Tests/Integration.hs` patterns
- **Test Pattern**:
```haskell
testMCPIntegration :: TestTree
testMCPIntegration = testGroup "MCP Tools"
  [ testContract' "mcp/inject.sol" Nothing Nothing Nothing True FuzzWorker
      [ ("injected tx executed", checkCorpusContains injectedTx) ]
  ]
```

**Source**: Existing Echidna test suite (tasty framework) + pytest documentation

---

## 8. Dependency Resolution

### Confirmed Dependencies
- **haskell-mcp-server** (v8b911269): Already in `stack.yaml` extra-deps
- **langchain**: Required in Python test environment
- **langchain_mcp_adapters**: For Python MCP client
- **pytest**: For test execution

### No Additional Haskell Dependencies Required
- STM: Already in `package.yaml` (`stm` library)
- Aeson: Already in use for JSON serialization
- Network.Wai: Already in use for SSE server

### Python Environment
```bash
# echidna_agent/requirements.txt
langchain==0.3.13
langchain-openai==0.2.14
langchain_mcp_adapters==0.1.0
pytest==8.3.4
httpx==0.28.1
```

**Source**: stack.yaml, package.yaml, langchain documentation

---

## 9. Logging & Reproducibility Research

### Decision: Structured Logging for All MCP Commands
- **Rationale**: Constitution principle - "Reproducibility & Determinism"
- **Mitigation Required**: All control commands (inject_transaction, prioritize_function) must be logged

### Implementation Strategy
1. Add `MCPCommandLog` type to track all MCP invocations
2. Write logs to `corpus-dir/mcp-commands.jsonl`
3. Replay capability: Parse logs and re-inject commands

### Log Format
```jsonl
{"timestamp": "2025-12-18T10:30:45Z", "tool": "inject_transaction", "args": {"tx": "transfer(0x123, 100)"}, "result": "success"}
{"timestamp": "2025-12-18T10:31:12Z", "tool": "prioritize_function", "args": {"sig": "balanceOf(address)"}, "result": "success"}
```

**Source**: Constitution requirement + Echidna corpus logging patterns

---

## 10. Code Organization Research

### Decision: Extend lib/Echidna/MCP.hs Module
- **Rationale**: Module already exists with tool scaffolding
- **Structure**:
```
lib/Echidna/MCP.hs (main module)
  - Tool definitions (10 tools)
  - JSON-RPC handling
  - Bus integration
  - Logging infrastructure

lib/Echidna/Types/InterWorker.hs (extend)
  - Add new FuzzerCmd constructors:
    - InjectTransaction Tx
    - PrioritizeFunctionCmd String

lib/Echidna/Worker.hs (extend)
  - Handle new Bus messages
  - Integrate prioritization logic

tests/mcp/ (new directory)
  - Python test scripts
  - Integration test contracts
```

**Source**: Existing Echidna module organization

---

## 11. Unknown/Clarification Items - RESOLVED

### ✅ Transaction Format (CLARIFIED)
- **Resolution**: Solidity-like syntax `transfer(0x123, 100)`
- **Documented**: specs/001-mcp-agent-commands/spec.md Clarification #1

### ✅ Prioritization Mechanism (CLARIFIED)
- **Resolution**: 90% prioritized, 10% random exploration
- **Documented**: specs/001-mcp-agent-commands/spec.md Clarification #2

### ✅ MCP Protocol Transport (RESEARCHED)
- **Resolution**: HTTP + SSE (Server-Sent Events)
- **Evidence**: Echidna already implements SSE in `lib/Echidna/Server.hs`

### ✅ Bus vs EventQueue Usage (RESEARCHED)
- **Resolution**: 
  - EventQueue (Chan): Read-only for observability tools
  - Bus (TChan): Write for control commands
- **Evidence**: Code inspection of `lib/Echidna/Types/Config.hs` and `lib/Echidna/Campaign.hs`

---

## 12. Risk Analysis

### High Risk Items
1. **Transaction Parsing Complexity**
   - **Risk**: ABI parsing errors could crash fuzzer
   - **Mitigation**: Validate all inputs, return error messages (never crash)

2. **Thread Safety with Bus Writes**
   - **Risk**: Race conditions when multiple MCP clients write to bus
   - **Mitigation**: STM guarantees atomicity, use `atomically` for all bus writes

3. **Performance Degradation**
   - **Risk**: MCP tool calls slow down fuzzing campaign
   - **Mitigation**: Async processing, <100ms response SLA, <1% overhead target

### Medium Risk Items
1. **Python Agent Timeout Handling**
   - **Risk**: Long-running MCP calls hang test scripts
   - **Mitigation**: Implement 30s timeout in Python client

2. **Log File Growth**
   - **Risk**: MCP command logs grow unbounded
   - **Mitigation**: Rotate logs per campaign session

### Low Risk Items
1. **Version Compatibility**
   - **Risk**: langchain API changes break test scripts
   - **Mitigation**: Pin versions in requirements.txt

---

## Next Steps (Phase 1)

1. **Generate data-model.md**: Define all data structures
   - MCPTool schema
   - Message extensions (FuzzerCmd additions)
   - WorkerState extensions (prioritizedFunctions field)
   - LogBuffer structure

2. **Generate contracts/**: OpenAPI-style schemas for 10 tools
   - Input schemas (JSON-RPC parameters)
   - Output schemas (JSON-RPC results)
   - Error response formats

3. **Generate quickstart.md**: Usage documentation
   - Installation steps
   - Starting MCP server
   - Example Python client code
   - Troubleshooting guide

4. **Update Agent Context**: Run `.specify/scripts/bash/update-agent-context.sh copilot`
   - Add haskell-mcp-server usage
   - Add langchain/langgraph patterns
   - Add STM/TChan examples

5. **Re-evaluate Constitution Check**: Confirm mitigation strategies

---

## Research Sources

1. **LangSmith Documentation** (Archon RAG)
   - https://docs.langchain.com/langsmith/trace-with-langgraph
   - @traceable decorator patterns
   - StateGraph workflow examples

2. **Echidna Codebase** (Semantic Search)
   - lib/Echidna/Campaign.hs: Event system, spawnListener
   - lib/Echidna/Types/Config.hs: Env type, eventQueue, Bus
   - lib/Echidna/Types/InterWorker.hs: Bus message protocol
   - lib/Echidna/MCP.hs: Existing MCP infrastructure
   - lib/Echidna/Server.hs: SSE implementation

3. **Haskell Ecosystem**
   - STM library documentation (implicit from code patterns)
   - Network.Wai.EventSource (SSE)

4. **MCP Protocol**
   - Standard: HTTP + SSE + JSON-RPC
   - haskell-mcp-server library (stack.yaml dependency)

---

## Approval

**Research Phase Complete**: ✅  
**All NEEDS CLARIFICATION Items Resolved**: ✅  
**Ready for Phase 1 (Design)**: ✅

**Reviewer**: Daniel Tradito  
**Date**: 2025-12-18

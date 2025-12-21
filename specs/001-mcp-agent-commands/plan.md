# Implementation Plan: MCP Agent Commands

**Branch**: `001-mcp-agent-commands` | **Date**: 2025-12-20 | **Spec**: [spec.md](spec.md)  
**Input**: Feature specification from `/specs/001-mcp-agent-commands/spec.md`

## Summary

Implement Model Context Protocol (MCP) integration for Echidna fuzzer, enabling AI agents to observe fuzzing campaigns via 6 observability tools (read_logs, show_coverage, dump_lcov, get_corpus_size, inspect_corpus_transactions, find_transaction_in_corpus) and control fuzzing via 3 control tools (inject_transaction, prioritize_function, clear_priorities). The MCP server uses HTTP + SSE + JSON-RPC transport, extends Echidna's existing Bus/EventQueue architecture, and ensures reproducibility through command logging. Technical approach: Haskell core (GHC 9.8) with haskell-mcp-server library, Python 3.12 test scripts using LangChain/LangGraph agent frameworks.

## Technical Context

**Language/Version**: Haskell (GHC 9.8), Python 3.12 for test scripts  
**Primary Dependencies**: haskell-mcp-server (v8b911269), langchain, langgraph, pytest, STM/TChan for concurrency  
**Storage**: JSONL file for MCP command log (`{corpus-dir}/mcp-commands.jsonl`), EventLog ring buffer (IORef, 2500 entries)  
**Testing**: tasty (Haskell unit/integration tests), pytest (Python MCP client tests)  
**Target Platform**: Linux/macOS server (Echidna fuzzer)  
**Project Type**: Single Haskell project with Python test suite  
**Performance Goals**: <100ms mean MCP tool response time (p95 <150ms, SC-008), <1% fuzzing overhead (SC-009), 2500-entry EventLog capacity (Clarification #4)  
**Constraints**: Thread-safe Bus communication (STM), reproducible campaigns (command logging, FR-010), 90% prioritization weight (Clarification #2), MCP server on configurable port via --mcp-port flag  
**Scale/Scope**: 9 MCP tools, 4 parallel fuzzing workers (default), agents can inject up to 100 transactions per call

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

**Evaluation**: ✅ PASS (with mitigation)

| Principle | Status | Notes |
|-----------|--------|-------|
| I. Property-Based Testing Core | ✅ PASS | MCP tools enhance observability without altering core fuzzing logic. Control commands (inject_transaction, prioritize_function) augment existing mutation strategies. |
| II. Modularity & Extensibility | ✅ PASS | MCP integration isolated in `lib/Echidna/MCP.hs`, extends existing Bus architecture. New tools register via `MCPTool` type (data-model.md Entity #1). |
| III. Reproducibility & Determinism | ⚠️ CONDITIONAL PASS | **Mitigation Required**: All control commands (inject_transaction, prioritize_function, clear_priorities) MUST be logged to `{corpus-dir}/mcp-commands.jsonl` (FR-010, Clarification #6). Replay via `--replay-mcp-commands` flag ensures deterministic campaign reproduction. |
| IV. Developer Experience (DX) | ✅ PASS | MCP tools provide actionable JSON responses with clear error messages. Quickstart guide (quickstart.md) enables 15-minute integration. OpenAPI spec (contracts/mcp-tools-openapi.json) facilitates tooling. |
| V. Security & Correctness | ✅ PASS | MCP tools read-only by default (observability). Control commands validated against input schemas. No external network access (localhost HTTP only). |

**Mitigation Implementation**:
- FR-010: Command logging requirement
- Tasks T012, T041, T047, T052: Implement logMCPCommand Bus message and JSONL persistence
- Clarification #6: Specifies log file location and JSONL format

## Project Structure

### Documentation (this feature)

```text
specs/001-mcp-agent-commands/
├── plan.md              # This file (Phase 1 complete)
├── research.md          # Phase 0 complete (495 lines, 12 sections)
├── spec.md              # Feature specification (250 lines, 6 clarifications resolved)
├── data-model.md        # Phase 1 complete (7 entities, relationships, validation)
├── quickstart.md        # Phase 1 complete (15-min integration guide)
├── tasks.md             # Phase 2 (76 tasks across 6 phases, NOT created by /speckit.plan)
├── contracts/           # Phase 1 complete
│   ├── mcp-tools-openapi.json  # OpenAPI 3.1 spec for 9 tools
│   └── tool-schemas.md         # JSON Schema definitions with examples
└── checklists/
    └── requirements.md  # Specification validation checklist (all items passing)
```

### Source Code (Echidna repository root)

```text
lib/Echidna/
├── MCP.hs                       # [EXTEND] Add 9 tool handlers, registry, spawnMCPServer
├── Types/
│   ├── MCP.hs                   # [NEW] MCPTool, EventLog, MCPCommandLog, ToolExecution types
│   ├── InterWorker.hs           # [EXTEND] Add InjectTransaction, LogMCPCommand to FuzzerCmd
│   ├── Config.hs                # [EXTEND] Add eventLog, mcpCommandLog, mcpServer to Env
│   └── Campaign.hs              # [EXTEND] Add prioritizedFunctions, priorityWeight to GenDict
├── Campaign.hs                  # [EXTEND] EventLog population from eventQueue consumer
├── Server.hs                    # [EXTEND] MCP server initialization in runSSEServer
├── Worker.hs                    # [EXTEND] Bus message handling (InjectTransaction, LogMCPCommand)
└── UI.hs                        # [EXTEND] Display MCP server status in TUI

tests/mcp/                       # [NEW] MCP integration test suite
├── contracts/                   # Solidity test contracts
│   ├── SimpleToken.sol          # ERC20-like contract for testing
│   └── Vault.sol                # Reentrancy-vulnerable contract
├── scripts/                     # Python MCP client scripts
│   ├── coverage_monitor.py      # Observability tool tests
│   ├── injection_test.py        # Control tool tests
│   └── agent_workflow.py        # LangGraph multi-step agent
├── conftest.py                  # pytest fixtures (MCP client, Echidna process)
├── test_observability_tools.py  # Unit tests for 6 observability tools
└── test_control_tools.py        # Unit tests for 3 control tools

echidna_agent/                   # Python virtual environment (existing)
├── bin/                         # Python 3.12 executables
└── lib/python3.12/site-packages/
    ├── langchain/               # [INSTALL] LangChain framework
    ├── langgraph/               # [INSTALL] LangGraph StateGraph
    ├── httpx/                   # [INSTALL] HTTP client for MCP calls
    └── pytest/                  # [INSTALL] Test framework
```

**Structure Decision**: Single Haskell project with isolated MCP module (`lib/Echidna/MCP.hs`). Python test suite in `tests/mcp/` for agent integration testing. Extends existing Bus architecture rather than creating separate service. Rationale from research.md: "Echidna already has foundational MCP integration (`lib/Echidna/MCP.hs`) with Bus architecture and tool infrastructure. Our task is to extend this existing foundation."

**Key Integration Points**:
1. **Bus (InterWorker.hs)**: Central message queue extended with 2 new commands (InjectTransaction, LogMCPCommand)
2. **EventLog (Types/MCP.hs)**: New ring buffer populated from existing `eventQueue :: Chan (LocalTime, CampaignEvent)`
3. **GenDict (Campaign.hs)**: Existing fuzzer dictionary extended with `prioritizedFunctions` field
4. **MCP Server (MCP.hs)**: Tool registry exposes 9 tools via HTTP + SSE, listens on configurable port (--mcp-port flag)

## Complexity Tracking

> **No violations requiring justification**

The Constitution Check passed with mitigation (command logging). No architectural complexity violations detected. MCP integration follows existing patterns (Bus for inter-worker communication, IORef for shared state, TChan for concurrency). Tool count (9 tools) justified by feature requirements (6 observability + 3 control = minimal complete set for agent workflows).

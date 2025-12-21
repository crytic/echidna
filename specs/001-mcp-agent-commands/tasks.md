# Tasks: MCP Commands for Agentic Testing

**Feature**: 001-mcp-agent-commands  
**Branch**: `001-mcp-agent-commands`  
**Planning Date**: 2025-12-20  
**Status**: Ready for Implementation

---

## Overview

This document defines all implementation tasks organized by user story. Each task is independently executable and follows a strict checklist format.

**Task Organization**:
- Phase 1: Setup & Infrastructure
- Phase 2: Foundational (Blocking)
- Phase 3: User Story 1 (Observability - P1)
- Phase 4: User Story 2 (Control - P1)
- Phase 5: User Story 3 (Testing - P2)
- Phase 6: Polish & Integration

**Parallelization Strategy**:
- Tasks marked [P] can run in parallel (different files, no dependencies on incomplete work)
- Most Phase 3 observability tasks are parallelizable (independent tools)
- Phase 4 control tasks are mostly sequential (depend on Bus extensions)
- Phase 5 testing can begin once Phase 3 observability is complete

---

## Phase 1: Setup & Infrastructure

### Checkpoint: Project initialization complete, dependencies verified, module structure established

- [X] T001 Initialize MCP module structure in `lib/Echidna/MCP.hs` with placeholder tool registry and import structure per [research.md Section 10](research.md#10-code-organization-research)
- [X] T002 [P] Update `lib/Echidna/Types/InterWorker.hs` to add `InjectTransaction Tx` and `LogMCPCommand` constructors to `FuzzerCmd` enum for control commands
- [X] T003 [P] Extend `lib/Echidna/Types/Campaign.hs` `GenDict` type to include `prioritizedFunctions :: Set Text` field (initially empty set)
- [X] T004 [P] Create `lib/Echidna/Types/MCP.hs` module defining `MCPTool` type, `ToolInput/ToolOutput` JSON schemas, and `MCPCommandLog` structure
- [X] T005 Create `tests/mcp/` directory structure: `tests/mcp/contracts/`, `tests/mcp/scripts/`, `tests/mcp/conftest.py`
- [X] T006 [P] Set up Python environment in `echidna_agent/mcp_env.txt` or update `echidna_agent/requirements.txt` with langchain, pytest, httpx dependencies (versions pinned)
- [X] T007 Create `tests/mcp/conftest.py` pytest fixture providing `mcp_client` fixture connecting to `localhost:8080` MCP server

---

## Phase 2: Foundational (Blocking Prerequisites)

### Checkpoint: All blocking infrastructure complete, Env extended, Bus ready for message passing

- [X] T008 Extend `lib/Echidna/Types/Config.hs` `Env` type to add `mcpCommandLog :: IORef [MCPCommandLog]` and `mcpServer :: Maybe ThreadId` fields
- [X] T009 Implement `spawnMCPServer :: Word16 -> Env -> IO ThreadId` in `lib/Echidna/Server.hs` (stub function, returns thread ID, logs "MCP server starting on port")
- [X] T010 [P] Implement Bus writer function `sendFuzzerCmd :: AgentId -> FuzzerCmd -> Env -> IO Bool` in `lib/Echidna/MCP.hs` (atomically writes to bus TChan, returns success)
- [X] T011 [P] Implement Bus reader integration in `lib/Echidna/Worker.hs` to handle new `FuzzerCmd` constructors (`InjectTransaction`, `LogMCPCommand`) in main worker loop
- [X] T012 [P] Create logging function `logMCPCommand :: Text -> [(Text, Text)] -> Text -> Env -> IO ()` that appends to `env.mcpCommandLog` with timestamp and tool name
- [X] T013 Integrate MCP server startup in `lib/Echidna/UI.hs` `ui` function: conditionally spawn MCP server if `cfg.campaignConf.mcpPort` is present (before worker spawn)
- [X] T014 Add command-line option `--mcp-port` to `src/Main.hs` parser to enable MCP server (optional, default None)

---

## Phase 3: User Story 1 - Observability (P1)

### Story Goal
Enable AI agents to observe fuzzing campaign state (logs, coverage, corpus) via MCP tools without affecting campaign execution.

### Independent Test Criteria
- Agent can call `read_logs` and receive recent campaign events
- Agent can call `show_coverage` and receive coverage statistics
- Agent can browse corpus with `get_corpus_size` and `inspect_corpus_transactions`
- All observability queries complete in <100ms and don't block fuzzing

### Task Dependencies
- Depends on: Phase 2 (foundational infrastructure)
- Unblocks: Phase 5 (testing - Python scripts can test these tools)

---

#### Tool 1: read_logs (Read Recent Campaign Events)

- [X] T015 [P] [US1] Implement `readLogsTool :: ToolExecution` in `lib/Echidna/MCP.hs` to read last 100 events from `env.eventQueue` using `readIORef env.eventLog` (stub: return mock JSON array)
- [X] T016 [P] [US1] Create `lib/Echidna/Types/MCP.hs` type `EventLog = [EventEntry]` where `EventEntry` has `timestamp`, `eventType`, `workerId`, `data` fields (JSON serializable)
- [X] T017 [P] [US1] Implement ring buffer for event logging: add `eventLog :: IORef EventLog` to `Env`, maintain max 2500 events (Clarification #4) via `atomicModifyIORef'` in `Worker.hs` event handler with drop-oldest behavior
- [X] T018 [P] [US1] Add JSON serialization instance for `EventLog` in `lib/Echidna/Orphans/JSON.hs` (or inline in MCP.hs if small)
- [ ] T019 [US1] Validate `readLogsTool` response includes fields: `events` (array), `count` (int), `timestamp` (ISO 8601) - per schema in contracts/read_logs.json (to be created in Phase 1)

#### Tool 2: show_coverage (Get Coverage Statistics)

- [X] T020 [P] [US1] Implement `showCoverageTool :: ToolExecution` in `lib/Echidna/MCP.hs` to call `mergeCoverageMaps env.dapp env.coverageRefInit env.coverageRefRuntime` and return JSON
- [X] T021 [P] [US1] Coverage JSON format: `{ "points": int, "bytecode": int, "contracts": { "0xAddr": { "lines": [int], "functions": { "sig": { "executed": bool } } } } }`
- [ ] T022 [P] [US1] Add `#[derive(ToJSON)]` or manual instance for coverage map in `lib/Echidna/Output/JSON.hs` if not present
- [ ] T023 [US1] Validate coverage data can be retrieved while campaign is running (test with tasty in `src/test/Tests/Integration.hs`)

#### Tool 3: dump_lcov (Export Coverage in LCOV Format)

- [X] T024 [P] [US1] Implement `dumpLcovTool :: ToolExecution` in `lib/Echidna/MCP.hs` to call existing `saveLcovHook` function from `lib/Echidna/Output/Source.hs`
- [X] T025 [P] [US1] Return `{ "format": "lcov", "content": "<multiline LCOV data>" }` as JSON response
- [X] T026 [P] [US1] Ensure tool doesn't write to disk (return in-memory string, not file path) - agents can post-process if needed

#### Tool 4: get_corpus_size (Get Corpus Count)

- [X] T027 [P] [US1] Implement `getCorpusSizeTool :: ToolExecution` in `lib/Echidna/MCP.hs` reading `env.corpusRef` via `readIORef`
- [X] T028 [P] [US1] Return `{ "size": int }` JSON (count only, detailed statistics provided by inspect_corpus_transactions tool)
- [X] T029 [P] [US1] Validation: Ensure size matches `length corpus` (simple count, no additional calculations needed for this tool)

#### Tool 5: inspect_corpus_transactions (Browse Corpus Transactions)

- [X] T030 [P] [US1] Implement `inspectCorpusTransactionsTool :: ToolExecution` in `lib/Echidna/MCP.hs` with parameters `page` (default 0) and `pageSize` (default 10)
- [X] T031 [P] [US1] Return `{ "transactions": [{ "index": int, "sequence_length": int, "coverage_delta": int, "sample_calls": [{ "function": str, "args": [value] }] }], "total": int, "page": int }`
- [X] T032 [P] [US1] Paginate corpus via `zip [0..] (toList corpusSet) & drop (page * pageSize) & take pageSize`
- [ ] T033 [US1] Test pagination: verify page size enforcement, boundary conditions (last page with <pageSize items)

#### Tool 6: find_transaction_in_corpus (Search Corpus by Function Signature)

- [X] T034 [P] [US1] Implement `findTransactionInCorpusTool :: ToolExecution` in `lib/Echidna/MCP.hs` with parameter `function_signature` (text)
- [X] T035 [P] [US1] Filter corpus transactions: for each `[Tx]` sequence, check if any `Tx.call` matches signature via string comparison
- [X] T036 [P] [US1] Return `{ "found": bool, "transactions": [{ "index": int, "sequence": [...] }] }` (limit to 10 results)
- [X] T037 [US1] Validate signature matching: case-insensitive, supports partial matches (e.g., "balanceOf" matches "balanceOf(address)")

---

## Phase 4: User Story 2 - Control (P1)

### Story Goal
Enable AI agents to influence campaign behavior through command injection and function prioritization.

### Independent Test Criteria
- Agent can inject transaction sequences that get executed by fuzzer
- Agent can prioritize functions and verify increased call frequency
- Clear prioritization works and resets call weights
- Injected transactions are logged for reproducibility

### Task Dependencies
- Depends on: Phase 2 (Bus infrastructure), Phase 3 observability (to verify effects)
- Unblocks: Phase 5 (testing)

---

#### Tool 7: inject_transaction (Inject Transaction Sequence)

- [X] T038 [P] [US2] Implement `injectTransactionTool :: ToolExecution` in `lib/Echidna/MCP.hs` with parameter `transactions` (JSON array of Solidity-like syntax strings)
- [X] T039 [P] [US2] Parser: for each string like `"transfer(0x123..., 100)"`, parse function signature and arguments using existing `Echidna.ABI.encodeSig` and `parseAbiValue` (from hevm)
- [X] T040 [P] [US2] Construct `[Tx]` sequence and send via `sendFuzzerCmd env (ToFuzzer workerId (InjectTransaction txSeq))`
- [X] T041 [P] [US2] Log injected transaction via `logMCPCommand "inject_transaction" [("tx_count", show count), ("sample_sig", sigStr)] "injected" env`
- [X] T042 [P] [US2] Return `{ "injected": bool, "transaction_count": int, "worker_id": int }` (verify bus write success)
- [ ] T043 [US2] Add integration test: inject known transaction (e.g., constructor call), verify it appears in corpus within 5 seconds via `find_transaction_in_corpus`

#### Tool 8: prioritize_function (Set Function Priority)

- [X] T044 [P] [US2] Implement `prioritizeFunctionTool :: ToolExecution` in `lib/Echidna/MCP.hs` with parameter `function_signature` (text, e.g., "balanceOf(address)")
- [X] T045 [P] [US2] Modify worker's `GenDict`: add signature to `prioritizedFunctions` set via atomic ref update
- [X] T046 [P] [US2] Modify `Echidna.Transaction` function selection in `genInteractionsM`: when selecting function, check if any signature in `genDict.prioritizedFunctions`, if yes choose with 90% probability, else uniform random
- [X] T047 [P] [US2] Log prioritization: `logMCPCommand "prioritize_function" [("function", funcSig)] "priority_set" env`
- [X] T048 [P] [US2] Return `{ "prioritized": bool, "function_signature": str, "worker_ids": [int] }` (list of affected workers)
- [ ] T049 [US2] Test prioritization: inject transactions calling non-prioritized function X, prioritize X, verify call frequency increases (via `inspect_corpus_transactions` delta)

#### Tool 9: clear_priorities (Reset Function Prioritization)

- [X] T050 [P] [US2] Implement `clearPrioritiesTool :: ToolExecution` in `lib/Echidna/MCP.hs` with no parameters
- [X] T051 [P] [US2] Clear `prioritizedFunctions` set in all `WorkerState` records via atomic update
- [X] T052 [P] [US2] Log: `logMCPCommand "clear_priorities" [] "cleared" env`
- [X] T053 [P] [US2] Return `{ "cleared": bool, "worker_ids": [int] }`
- [ ] T054 [US2] Validate clearing works: after clearing, function call frequency returns to uniform distribution

---

## Phase 5: User Story 3 - Testing (P2)

### Story Goal
Provide comprehensive test suite for MCP integration including Python client scripts and integration tests.

### Independent Test Criteria
- Python test scripts can connect to live MCP server
- Each tool can be invoked and returns expected JSON schema
- Multi-step agent workflows work (e.g., read_logs → prioritize_function → read_logs)
- All response times <100ms

### Task Dependencies
- Depends on: Phase 3 (observability tools), Phase 4 (control tools)
- Blocking: Release

---

#### Python Test Infrastructure

- [X] T055 [P] [US3] Create `tests/mcp/conftest.py` with `mcp_client` fixture and `echidna_campaign_running` fixture (spawns Echidna with `--server-port 8080`)
- [X] T056 [P] [US3] Create `tests/mcp/scripts/mcp_client_wrapper.py` with `MCPClientV2` class wrapping httpx, methods for each tool (call_read_logs, call_show_coverage, etc.)
- [X] T057 [P] [US3] Add schema validation: `validate_response(response, schema_name)` function using jsonschema library for contract validation

#### Observability Tool Tests

- [X] T058 [P] [US3] Create `tests/mcp/test_read_logs.py` with test cases:
  - `test_read_logs_initial` - verify logs exist on fresh campaign
  - `test_read_logs_pagination` - verify can request different time ranges
  - `test_read_logs_response_time` - verify <100ms execution
- [X] T059 [P] [US3] Create `tests/mcp/test_coverage.py` with cases:
  - `test_show_coverage_empty` - verify coverage data exists even with no tests
  - `test_coverage_increases` - run campaign for N seconds, verify coverage points increase
  - `test_dump_lcov_format` - verify LCOV format correctness (LCOV grammar)
- [X] T060 [P] [US3] Create `tests/mcp/test_corpus.py` with cases:
  - `test_get_corpus_size` - verify corpus size > 0 after campaign runs
  - `test_inspect_corpus_pagination` - verify pagination parameters work
  - `test_find_transaction_in_corpus` - inject known tx, find via tool

#### Control Tool Tests

- [X] T061 [P] [US3] Create `tests/mcp/test_injection.py` with cases:
  - `test_inject_single_transaction` - inject valid Solidity syntax, verify execution
  - `test_inject_invalid_transaction` - inject malformed input, verify error response
  - `test_inject_response_time` - verify <100ms response
- [X] T062 [P] [US3] Create `tests/mcp/test_prioritization.py` with cases:
  - `test_prioritize_function` - set priority, verify via call frequency
  - `test_clear_priorities` - clear and verify frequency returns to normal
  - `test_priority_persistence` - verify priority survives across multiple calls
- [X] T063 [P] [US3] Create `tests/mcp/test_integration_workflows.py` with multi-step scenarios:
  - `test_agent_workflow_observe_then_inject` - read logs → inject tx → read logs again
  - `test_agent_workflow_prioritize_and_monitor` - prioritize → get corpus → find transactions
  - `test_concurrent_tool_calls` - call 3+ tools concurrently, verify no races

#### Contract/Schema Tests

- [X] T064 [P] [US3] Create `tests/mcp/test_schemas.py` validating each tool response against schema:
  - `test_read_logs_schema` - validate response has required fields
  - `test_coverage_schema` - validate coverage JSON structure
  - `test_injection_schema` - validate injection response format
  - Uses jsonschema to validate (schemas in contracts/*.json)
- [X] T065 [P] [US3] Create JSON schema files in `contracts/` directory (created in Phase 1 planning):
  - `contracts/read_logs.json` - schema for read_logs response
  - `contracts/show_coverage.json` - schema for coverage response
  - (all 10 tool schemas, ~10 lines each)

#### Integration Test Contracts

- [X] T066 [P] [US3] Create `tests/mcp/contracts/SimpleToken.sol` with:
  - `transfer(address to, uint256 amount)` function (injectable via MCP)
  - `balanceOf(address)` function (for prioritization testing)
  - `approve(address, uint256)` function (for multi-function testing)
- [X] T067 [P] [US3] Create `tests/mcp/contracts/EchidnaMCPTest.sol` with property tests:
  - `echidna_balances_conserved() -> bool` (basic property)
  - `echidna_no_overflow() -> bool` (for prioritization impact)

#### Haskell Integration Tests

- [X] T068 [US3] Add tasty tests in `src/test/Tests/Integration.hs`:
  - `testMCPServer` - spawn Echidna with MCP server, verify server startup log
  - `testMCPToolsWithFuzzWorker` - run contract with `--mcp-port`, verify tools respond
  - **Acceptance Criteria**: (1) Server spawns without crash, (2) Tools respond within 100ms mean (150ms p95), (3) Server shutdown is graceful with no hung processes
  - (these are integration tests, ~20-50 lines per test)

---

## Phase 6: Polish & Integration

### Checkpoint: All features working, integration complete, ready for PR

- [ ] T069 [P] Update `CHANGELOG.md` to document MCP tool addition (section: "Added")
- [ ] T070 [P] Update `README.md` with MCP server usage documentation (new section: "Agent Integration")
- [ ] T071 Verify all code compiles: `stack build` (should succeed with no warnings in new code)
- [ ] T072 Run test suite: `stack test` (existing tests + new MCP tests pass)
- [ ] T073 Performance validation: manual test to verify <1% fuzzing overhead with MCP server running
- [ ] T074 Manual integration test: run `echidna tests/mcp/contracts/SimpleToken.sol --mcp-port 8080` and connect Python client. Optional enhancement: test with live Anvil node via `echidna tests/mcp/contracts/SimpleToken.sol --mcp-port 8080 --rpc-url http://127.0.0.1:8545` for realistic network testing
- [ ] T075 Documentation: ensure `contracts/*.json` schemas are readable and serve as API reference
- [ ] T076 Git cleanup: verify no debug code, remove unused imports, ensure code style matches Echidna conventions

---

## Task Dependency Graph

```
Phase 1 (Setup)
  ├─ T001: Module init
  ├─ T002-T004: Type definitions [P]
  ├─ T005-T006: Test infra [P]
  └─ T007: Python fixture

        ↓
Phase 2 (Blocking)
  ├─ T008-T009: Env extensions
  ├─ T010-T012: Bus integration [P]
  ├─ T013-T014: CLI integration
  
        ↓ (depends on Phase 2)
Phase 3 (Observability - P1) [MOSTLY PARALLEL]
  ├─ T015-T019: read_logs [P]
  ├─ T020-T023: show_coverage [P]
  ├─ T024-T026: dump_lcov [P]
  ├─ T027-T029: get_corpus_size [P]
  ├─ T030-T033: inspect_corpus [P]
  └─ T034-T037: find_transaction [P]

        ↓ (depends on Phase 2)
Phase 4 (Control - P1) [SEQUENTIAL]
  ├─ T038-T043: inject_transaction [P-independent]
  ├─ T044-T049: prioritize_function [sequential, depends on transaction infra]
  └─ T050-T054: clear_priorities [sequential]

        ↓ (depends on Phase 3+4)
Phase 5 (Testing - P2) [MOSTLY PARALLEL]
  ├─ T055-T057: Test infra [P]
  ├─ T058-T060: Observability tests [P]
  ├─ T061-T063: Control tests [P]
  ├─ T064-T065: Schema tests [P]
  └─ T066-T068: Contract tests [P]

        ↓
Phase 6 (Polish)
  ├─ T069-T070: Documentation [P]
  ├─ T071-T073: Validation [sequential]
  └─ T074-T076: Final review
```

---

## Parallelization Strategy & MVP Scope

### Recommended Execution Order

**Critical Path** (must complete sequentially):
1. Phase 1 → Phase 2 → Phase 3 → Phase 4 → Phase 5 → Phase 6

**Parallelizable Within Phases**:
- Phase 1: All T002-T006 can run in parallel
- Phase 3: All observability tools T015-T037 can run in parallel (independent tool implementations)
- Phase 4: T038-T043 (injection) can run in parallel with T044-T049 (prioritization) since they use Bus independently
- Phase 5: All test tasks marked [P] can run in parallel

### MVP Scope (Phase 1 + Subset of Phase 3 + Basic Testing)

For a minimal viable product, implement:
- Phase 1: Full setup ✅
- Phase 2: Full infrastructure ✅
- Phase 3: Observability (minimum: `read_logs`, `get_corpus_size`, `show_coverage`) - other tools optional
- Phase 4: Control (minimum: `inject_transaction`) - prioritization optional
- Phase 5: Basic testing (T055-T060: Python client + observability tests)

This MVP delivers: **Agent can read campaign state and inject transactions** (2 core user stories, P1+P1 subset).

### Full Feature Scope (All 6 Phases)

Includes all 9 tools, comprehensive testing, and Polish phase.

---

## Success Metrics

| Metric | Target | Validation Task |
|--------|--------|-----------------|
| All 9 MCP tools implemented | 100% | T015-T062 implementation completion |
| Tool response time | <100ms each | T043, T058-T063 response time assertions |
| Fuzzing throughput impact | <1% overhead | T073 manual performance test |
| Test coverage | ≥80% for MCP module | T068-T063 tasty + pytest completion |
| Code compiles | 0 errors | T071 `stack build` |
| All tests pass | 100% | T072 `stack test` + pytest runs |
| Agent integration works | ✅ | T074 manual Python client test |

---

## Notes for Implementation

1. **Solidity Transaction Parsing**: Use `Text.Read.readMaybe` + pattern matching or build a small parser. Examples:
   - Input: `"transfer(0x123..., 100)"` 
   - Parse: function name + parenthesized args
   - Use existing `Echidna.ABI.encodeSig` for signature encoding

2. **Bus Thread Safety**: Always use `atomically { writeTChan bus msg }` for thread-safe writes. STM guarantees atomicity.

3. **Logging Strategy**: Append to `IORef [MCPCommandLog]` using `atomicModifyIORef'` to prevent race conditions.

4. **Schema First**: Define JSON schemas in `contracts/*.json` before implementing response serialization.

5. **Error Handling**: All MCP tools must return JSON error responses (never crash the fuzzer). Example: `{ "error": "Invalid function signature", "code": "INVALID_INPUT" }`

---

## Estimated Effort

- **Phase 1**: 1-2 hours (setup, types)
- **Phase 2**: 2-3 hours (blocking infrastructure, Bus integration)
- **Phase 3**: 6-8 hours (6 observability tools, mostly parallelizable)
- **Phase 4**: 4-5 hours (3 control tools, prioritization complexity)
- **Phase 5**: 8-10 hours (test infrastructure + 10+ test suites)
- **Phase 6**: 1-2 hours (documentation, final validation)

**Total**: ~24-30 hours of development

---

## Checklist Format Reference

Each task follows this exact format:
```
- [ ] [TaskID] [Optional markers] Description with file path
```

Where:
- `- [ ]` = Unchecked checkbox (markdown)
- `[TaskID]` = Sequential ID (T001, T002, ...)
- `[P]` = Parallelizable (optional, only if independent)
- `[US#]` = User Story label (optional, for story phase tasks)
- **Description** = Specific action with file path

Example valid tasks:
- `- [ ] T001 Initialize MCP module in lib/Echidna/MCP.hs`
- `- [ ] T015 [P] [US1] Implement readLogsTool in lib/Echidna/MCP.hs`
- `- [ ] T050 [P] [US2] Clear prioritizedFunctions in Worker.hs`

---

**Approval**: Ready for implementation ✅  
**Last Updated**: 2025-12-20  
**Next Step**: Begin Phase 1 implementation

# Feature Specification: MCP Commands for Agentic Testing

**Feature Branch**: `001-mcp-agent-commands`  
**Created**: 2025-12-18  
**Status**: Approved  
**Input**: Based on PRs [#1502](https://github.com/crytic/echidna/pull/1502), [#1423](https://github.com/crytic/echidna/pull/1423), and Issue [#1490](https://github.com/crytic/echidna/issues/1490) - Create MCP commands to enable AI agents to observe and control Echidna fuzzing campaigns for test automation.

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Agent Observes Fuzzing Campaign (Priority: P1)

An AI agent connects to a running Echidna fuzzing campaign via MCP (Model Context Protocol) and observes its progress in real-time. The agent can read campaign logs, check coverage statistics, browse the corpus of discovered transactions, and export coverage data for analysis - all without interrupting the fuzzing process.

**Why this priority**: Observability is the foundation for agent-driven testing. Without being able to see what Echidna is doing, agents cannot make informed decisions about test strategies. This is the minimal viable feature that enables all future agent capabilities.

**Independent Test**: Can be fully tested by:
1. Starting Echidna with `--mcp-port 8080` on a sample contract
2. Using an MCP client to call `read_logs`, `show_coverage`, `get_corpus_size` tools
3. Verifying responses contain valid campaign state data
4. Confirming fuzzing continues uninterrupted (<1% overhead)

**Acceptance Scenarios**:

1. **Given** Echidna is fuzzing a contract with 3 workers, **When** agent calls `read_logs` tool, **Then** agent receives last 100 campaign events including test results, coverage updates, and worker status
2. **Given** fuzzing campaign has discovered 50 transactions with 80% coverage, **When** agent calls `show_coverage`, **Then** agent receives JSON with coverage points, bytecode coverage, and per-contract statistics
3. **Given** corpus contains 50 transaction sequences, **When** agent calls `inspect_corpus_transactions` with page=0, pageSize=10, **Then** agent receives first 10 sequences with metadata (length, coverage delta, sample function calls)
4. **Given** campaign is actively fuzzing, **When** agent calls `dump_lcov`, **Then** agent receives LCOV-formatted coverage data suitable for external analysis tools
5. **Given** agent wants to find all transactions calling `transfer(address,uint256)`, **When** agent calls `find_transaction_in_corpus` with signature="transfer", **Then** agent receives list of matching transactions from corpus

---

### User Story 2 - Agent Controls Fuzzing Strategy (Priority: P1)

An AI agent analyzes Echidna's progress and actively influences the fuzzing strategy by injecting specific transaction sequences (e.g., exploit attempts, edge cases) and prioritizing certain functions to focus fuzzing effort. All control commands are logged to maintain reproducibility per Echidna's constitution.

**Why this priority**: Control capability transforms agents from passive observers to active participants in testing. This enables sophisticated strategies like: "I see low coverage in the withdrawal function - let me prioritize it and inject edge case transactions to test reentrancy guards."

**Independent Test**: Can be fully tested by:
1. Agent observes low coverage in specific function via `show_coverage`
2. Agent calls `prioritize_function` with that function's signature
3. Agent calls `inject_transaction` with targeted test cases
4. Agent verifies via `inspect_corpus_transactions` that new transactions were executed and function call frequency increased
5. Agent confirms all commands logged via `read_logs` (reproducibility)

**Acceptance Scenarios**:

1. **Given** agent identifies suspicious function `emergencyWithdraw()`, **When** agent calls `inject_transaction` with malicious sequence `["deposit(1000)", "emergencyWithdraw()"]`, **Then** Echidna executes sequence within 5 seconds and agent can find it via `find_transaction_in_corpus`
2. **Given** agent wants to focus testing on access control, **When** agent calls `prioritize_function` with signature="onlyOwner", **Then** 90% of subsequent transactions call functions with onlyOwner modifier
3. **Given** agent has prioritized 3 functions, **When** agent calls `clear_priorities`, **Then** function call frequency returns to uniform random distribution
4. **Given** agent injects transaction with invalid Solidity syntax, **When** Echidna parses transaction, **Then** agent receives error response with clear message (never crashes fuzzer)
5. **Given** agent issues control command, **When** command executes, **Then** command is logged to MCP command log with timestamp and parameters for reproducibility

---

### User Story 3 - Agent Tests MCP Integration (Priority: P2)

Developers and AI researchers validate the MCP integration by running comprehensive test suites that verify all tools work correctly, handle edge cases gracefully, and meet performance requirements. Tests include Python scripts using langchain/langgraph for multi-step agent workflows and Haskell integration tests.

**Why this priority**: Testing ensures reliability and serves as documentation for agent developers. While lower priority than core functionality, it's essential for production readiness and developer confidence.

**Independent Test**: Can be fully tested by:
1. Running `pytest tests/mcp/` with Echidna server active
2. All test cases pass (observability, control, schema validation, workflows)
3. Response times measured <100ms per tool
4. Multi-step agent scenarios work (e.g., observe → inject → observe → verify)

**Acceptance Scenarios**:

1. **Given** MCP server is running, **When** Python test script calls all 9 tools sequentially, **Then** all responses match JSON schema contracts and complete in <100ms each
2. **Given** test suite includes multi-step workflow, **When** test executes "read_logs → find low-coverage function → prioritize_function → inject edge cases → verify increased coverage", **Then** workflow succeeds end-to-end
3. **Given** test injects malformed transaction, **When** tool validates input, **Then** returns JSON error response (not crash) with code and message
4. **Given** 3 MCP clients connect simultaneously, **When** all clients call tools concurrently, **Then** no race conditions occur and all responses are correct
5. **Given** Echidna campaign finishes, **When** MCP server shuts down gracefully, **Then** all command logs are persisted and no data is lost

---

### Edge Cases

- **Empty Corpus**: What happens when agent calls `inspect_corpus_transactions` before any transactions discovered? Return empty array with total=0.
- **Invalid Transaction Syntax**: Agent injects `"transfer(0xZZZ, -100)"` with invalid hex/negative value? Return error JSON: `{"error": "Invalid transaction syntax", "code": "PARSE_ERROR"}`.
- **Concurrent Prioritization**: Two agents prioritize different functions simultaneously? Last write wins, logged in order received.
- **Server Shutdown**: Agent calls tool while Echidna is terminating? Return error: `{"error": "Campaign stopped", "code": "CAMPAIGN_TERMINATED"}`.
- **Large Corpus Pagination**: Corpus has 10,000 transactions, agent requests page 999? Return empty array if beyond bounds.
- **Prioritize Non-Existent Function**: Agent prioritizes function not in contract? Accept command (no validation until fuzzer tries to call it), may have no effect if function never appears in corpus.
- **Inject Transaction to Stopped Worker**: Target worker has finished/crashed? Broadcast to all active workers or return error if none active.
- **LCOV Export During Active Fuzzing**: Coverage data may be incomplete/stale? Return current snapshot with warning in response metadata.
- **Multiple NEEDS CLARIFICATION Markers**: Spec has >3 clarification requests? AI must make informed guesses for all but top 3 critical decisions (scope, security, UX impact).

## Requirements *(mandatory)*

### Functional Requirements

**Observability Tools (Read-Only)**

- **FR-001**: System MUST provide `read_logs` tool returning last 100 campaign events (test results, coverage updates, worker status) in JSON format
- **FR-002**: System MUST provide `show_coverage` tool returning coverage statistics (points, bytecode coverage, per-contract breakdown) queryable during active fuzzing
- **FR-003**: System MUST provide `dump_lcov` tool exporting coverage data in LCOV format for external analysis tools
- **FR-004**: System MUST provide `get_corpus_size` tool returning corpus transaction count as integer (detailed statistics available via inspect_corpus_transactions)
- **FR-005**: System MUST provide `inspect_corpus_transactions` tool with pagination (page, pageSize) to browse discovered transactions with metadata (includes average/max sequence length, coverage points)
- **FR-006**: System MUST provide `find_transaction_in_corpus` tool accepting function signature to search corpus and return matching transaction sequences

**Control Tools (State-Modifying)**

- **FR-007**: System MUST provide `inject_transaction` tool accepting Solidity-like syntax (e.g., `"transfer(0x123..., 100)"`) to inject transaction sequences into active fuzzing campaign
- **FR-008**: System MUST provide `prioritize_function` tool accepting function signature to bias fuzzing toward that function (90% probability vs 10% random)
- **FR-009**: System MUST provide `clear_priorities` tool to reset all function prioritization and return to uniform random selection
- **FR-010**: System MUST log all control commands (inject, prioritize, clear) with timestamps to maintain reproducibility per Constitution Principle III

**Integration & Non-Functional**

- **FR-011**: System MUST expose MCP server on configurable port via `--mcp-port` CLI flag (default: disabled, recommended: 8080)
- **FR-012**: System MUST use HTTP + Server-Sent Events (SSE) transport for MCP protocol following JSON-RPC standard
- **FR-013**: System MUST handle invalid tool inputs gracefully returning JSON error responses (never crash fuzzer)
- **FR-014**: System MUST support concurrent MCP clients without race conditions using STM (Software Transactional Memory)
- **FR-015**: System MUST complete all tool calls in <100ms mean response time (p95 latency <150ms for robustness under load) to avoid blocking agent workflows

**Testing & Documentation**

- **FR-016**: System MUST include Python test scripts using pytest + langchain for all 9 tools (observability, control, integration)
- **FR-017**: System MUST include Haskell integration tests using existing tasty framework to verify MCP server lifecycle
- **FR-018**: System MUST provide JSON schema contracts for all tool inputs/outputs in `contracts/*.json` directory
- **FR-019**: System MUST maintain <1% fuzzing throughput overhead when MCP server is active but idle
- **FR-020**: System MUST update README with MCP usage examples and quickstart guide

### Key Entities

- **MCPTool**: Represents a callable tool with name, description, input schema, output schema, and execution function. Tools are registered in tool registry at server startup.

- **EventLog**: Ring buffer (max 2500 entries) of campaign events with fields: timestamp (LocalTime), eventType (string), workerId (int), data (JSON). Maintained in IORef for thread-safe reads. Drops oldest entries when capacity reached (Clarification #4).

- **MCPCommandLog**: Audit log of control commands with fields: timestamp, toolName, parameters (key-value pairs), result (success/error). Persisted to `{corpus-dir}/mcp-commands.jsonl` for reproducibility (Clarification #6).

- **Bus**: STM TChan-based message passing system between MCP server and fuzzing workers. Supports message types: InjectTransaction, PrioritizeFunctionCmd, ClearPriorities.

- **ToolExecution**: Function type `[(Text, Text)] -> Env -> Bus -> IORef [Text] -> IO String` representing tool execution signature (takes parameters, returns JSON string).

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: Agent can observe fuzzing progress via 6 observability tools, all responding in <100ms and returning valid JSON matching schema contracts
- **SC-002**: Agent can inject transaction and verify execution within 5 seconds by finding injected sequence in corpus via `find_transaction_in_corpus` tool
- **SC-003**: Agent can prioritize function and verify 90% of subsequent transactions call that function (measured via `inspect_corpus_transactions` over 100 transactions)
- **SC-004**: Fuzzing throughput degradation is <1% when MCP server active (measured by transactions/second with vs without --mcp-port flag)
- **SC-005**: All control commands logged to MCPCommandLog with 100% accuracy (no missing entries), verifiable by reading command log file
- **SC-006**: Concurrent MCP clients (tested with 3 simultaneous connections) can call tools without race conditions or data corruption
- **SC-007**: MCP server startup/shutdown is graceful with no crashes or hung processes (tested in 100 campaign lifecycles)
- **SC-008**: Invalid tool inputs return JSON error responses (not crashes) in 100% of error cases (tested with fuzzer generating malformed inputs)
- **SC-009**: Python test suite achieves 100% pass rate for all 9 tools across observability, control, schema validation, and integration workflow tests
- **SC-010**: Developer can integrate MCP agent in <30 minutes using quickstart.md documentation (measured via user testing with 3 developers unfamiliar with Echidna)

## Clarifications *(resolved during specification)*

### Clarification #1: Transaction Injection Format (RESOLVED)

**Context**: The `inject_transaction` tool needs to parse transaction strings provided by agents. Multiple format options exist.

**What we need to know**: What syntax should agents use to specify transactions?

**Options Considered**:

| Option | Answer | Implications |
|--------|--------|--------------|
| A | Solidity-like syntax: `transfer(0x123..., 100)` | Natural for LLM agents, requires custom parser, examples: `deposit(1000)`, `approve(0xABC, 999)` |
| B | ABI-encoded hex: `0xa9059cbb000000000000000000000000...` | No parsing needed, but opaque for agents, harder to generate |
| C | JSON structured: `{"function": "transfer", "args": ["0x123", 100]}` | Explicit structure, verbose, requires JSON schema validation |

**Resolution**: **Option A (Solidity-like syntax)** chosen because:
- LLM agents naturally generate Solidity code
- Human-readable for debugging
- Echidna already has ABI parsing in `Echidna.ABI` module to leverage
- Examples: `transfer(0x123..., 100)`, `approve(0xABC, 1000000)`, `withdraw()`

### Clarification #2: Function Prioritization Mechanism (RESOLVED)

**Context**: The `prioritize_function` tool needs to bias fuzzing toward specific functions without completely excluding others (to avoid missing bugs).

**What we need to know**: What probability distribution should be used when a function is prioritized?

**Options Considered**:

| Option | Answer | Implications |
|--------|--------|--------------|
| A | 90% prioritized, 10% random exploration | Strongly biased but maintains some exploration, proven in research literature |
| B | 100% prioritized (deterministic) | No exploration, risks missing bugs in unprioritized functions |
| C | 70% prioritized, 30% random | More balanced but may not focus enough for targeted testing |

**Resolution**: **Option A (90% prioritized, 10% random)** chosen because:
- Balances focused testing with exploration
- 10% random prevents "blind spots" in test coverage
- Aligns with fuzzing best practices (e.g., AFL's havoc mode)
- Agent can clear priorities and re-prioritize if strategy fails

### Clarification #3: Tool Count Confirmation (RESOLVED)

**Context**: Documentation mentions "10 MCP tools" but implementation shows 9 tools.

**What we need to know**: How many tools should be implemented?

**Resolution**: **9 tools total** confirmed:

**Observability (6 tools)**:
1. read_logs
2. show_coverage  
3. dump_lcov
4. get_corpus_size
5. inspect_corpus_transactions
6. find_transaction_in_corpus

**Control (3 tools)**:
7. inject_transaction
8. prioritize_function
9. clear_priorities

Note: Earlier documents incorrectly stated "10 tools" - this has been corrected throughout the specification.

### Clarification #4: EventLog Buffer Overflow Behavior (RESOLVED)

**Context**: The EventLog maintains a ring buffer for campaign events. When the buffer reaches capacity, it needs a strategy for handling new events.

**What we need to know**: How should the system behave when EventLog reaches its maximum capacity?

**Options Considered**:

| Option | Answer | Implications |
|--------|--------|--------------||
| A | Drop oldest entries (ring buffer) | Continuous logging, always shows recent events, may lose historical data |
| B | Stop accepting new entries | Preserves all early events but stops logging mid-campaign |
| C | Configurable size with overflow warning | Flexible but adds configuration complexity |

**Resolution**: **Option A (Drop oldest, increase capacity to 2500)** chosen because:
- Ring buffer ensures recent events always visible to agents
- 2500 entries provides ~5-10 minutes of history (typical campaign duration)
- Simple implementation using circular buffer or queue
- Aligns with log rotation best practices

**Implementation Note**: EventLog capacity increased from 1000 to 2500 entries.

### Clarification #5: Transaction Injection Worker Selection (RESOLVED)

**Context**: When an agent injects a transaction sequence, the system must decide which fuzzing worker(s) should execute it.

**What we need to know**: How should injected transactions be distributed across workers?

**Options Considered**:

| Option | Answer | Implications |
|--------|--------|--------------||
| A | Broadcast to all workers | Highest execution priority, redundant execution, faster verification |
| B | Round-robin distribution | Load balanced, single execution, may delay verification |
| C | Random worker selection | Simple, unpredictable timing, single execution |

**Resolution**: **Option A (Broadcast to all workers)** chosen because:
- Ensures injected transactions execute quickly (critical for agent workflows)
- All workers can learn from injected edge cases
- Redundancy is acceptable (injected txs are small fraction of total)
- Simpler than tracking "which worker executed this" for verification

**Implementation Note**: `inject_transaction` sends `InjectTransaction` message to all active workers via broadcast.

### Clarification #6: MCP Command Log Persistence Location (RESOLVED)

**Context**: Control commands (inject, prioritize, clear) must be logged for reproducibility. The log needs a persistent storage location.

**What we need to know**: Where should MCPCommandLog entries be persisted?

**Options Considered**:

| Option | Answer | Implications | Pros | Cons |
|--------|--------|--------------|------|------|
| A | Same location as corpus | Single directory for all reproducibility data | Centralized, simple discovery | May clutter corpus directory |
| B | Separate file in project root | Isolated, easy to find | Clear separation of concerns | Another location to remember |
| C | Subdirectory of corpus | Organized under corpus structure | Grouped with related data | Requires corpus-dir to be set |

**Resolution**: **Option A (Same location as corpus) with Option B evaluation** chosen because:

**Chosen: Option A - Same Location as Corpus**
- **Pros**:
  - Single `--corpus-dir` flag controls all reproducibility data
  - Natural grouping: corpus.zip + mcp-commands.jsonl = complete replay state
  - Simplifies documentation ("all campaign data in corpus-dir")
  - Aligns with existing Echidna patterns (corpus + coverage in same dir)
- **Cons**:
  - Corpus directory has multiple file types (may feel cluttered)
  - MCP commands conceptually separate from discovered transactions

**Evaluated: Option B - Separate File**
- **Pros**:
  - Clear separation: corpus = discovered data, mcp-commands = agent interventions
  - Easier to .gitignore corpus while tracking MCP commands (if desired)
  - More intuitive for users who think "corpus = transactions only"
- **Cons**:
  - Requires new CLI flag `--mcp-log-file` or hardcoded location
  - Split reproducibility data across two locations
  - Users must remember to copy both corpus AND mcp log for replay

**Decision Rationale**: Option A's centralization benefits outweigh Option B's separation clarity. Users already understand `--corpus-dir` as "all campaign state goes here". Adding MCP commands to that directory is intuitive extension.

**Implementation Note**: 
- File path: `{corpus-dir}/mcp-commands.jsonl` (JSONL format, one command per line)
- Format: `{"timestamp": "2025-12-20T10:30:45Z", "tool": "inject_transaction", "args": {...}, "result": "success"}`
- Replay: Tools can parse this file to reproduce agent interventions in order

---

**Specification Quality Checklist**: All items passed ✅  
**Clarifications Complete**: 6/6 resolved (3 critical + 3 refinement)  
**Approval Status**: Ready for implementation  
**Next Phase**: Planning (speckit.plan)


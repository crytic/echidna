# Quickstart Guide: MCP Agent Commands for Echidna

**Feature**: 001-mcp-agent-commands  
**Audience**: Developers integrating AI agents with Echidna  
**Time to complete**: 15 minutes

---

## Prerequisites

- **Echidna**: Built from source with MCP support (branch `001-mcp-agent-commands`)
- **Python**: 3.12+ with `langchain`, `langgraph`, `pytest` installed
- **Solidity**: Target smart contract for fuzzing
- **Tools**: `curl` or Python HTTP client for testing

---

## Quick Start (5 minutes)

### 1. Build Echidna with MCP Support

```bash
cd /Users/daniel.a.tradito/Development/Audit/echidna-mcp
stack build
```

### 2. Start Echidna with MCP Server

```bash
echidna MyContract.sol --test-mode property \
  --mcp-port 8080 \
  --corpus-dir ./corpus \
  --workers 4
```

**What happens**:
- Echidna starts fuzzing `MyContract.sol`
- MCP server listens on `http://localhost:8080`
- EventLog ring buffer (2500 entries) tracks campaign events
- 4 workers execute transactions in parallel

### 3. Test MCP Connection

```bash
# List available tools
curl http://localhost:8080/mcp/tools

# Read last 10 events
curl -X POST http://localhost:8080/mcp/tools/read_logs \
  -H "Content-Type: application/json" \
  -d '{"count": 10}'
```

**Expected output**:
```json
{
  "events": [
    {
      "timestamp": "2025-12-20T10:30:45",
      "eventType": "TransactionExecuted",
      "workerId": 2,
      "data": {"function": "transfer(address,uint256)", "gasUsed": 21000}
    }
  ],
  "totalCount": 523
}
```

---

## Agent Integration (Python + LangChain)

### Setup Python Environment

```bash
cd echidna_agent
python3.12 -m venv .
source bin/activate
pip install langchain langgraph httpx pytest
```

### Example Agent Script

```python
# tests/mcp/scripts/coverage_monitor.py
import httpx
import time
from langchain.tools import tool

@tool
def read_echidna_logs(count: int = 100) -> dict:
    """Read campaign event logs from Echidna MCP server."""
    response = httpx.post(
        "http://localhost:8080/mcp/tools/read_logs",
        json={"count": count}
    )
    return response.json()

@tool
def get_coverage() -> dict:
    """Get current code coverage statistics."""
    response = httpx.post(
        "http://localhost:8080/mcp/tools/show_coverage"
    )
    return response.json()

# Agent workflow
def coverage_monitoring_agent():
    """Monitor coverage and suggest function prioritization."""
    while True:
        # Check coverage every 30 seconds
        coverage = get_coverage()
        print(f"Overall coverage: {coverage['overallCoverage']}%")
        
        # Find low-coverage contracts
        for contract in coverage['contracts']:
            if contract['lineCoverage'] < 50:
                print(f"⚠️  Low coverage on {contract['name']}: {contract['lineCoverage']}%")
                # Agent could prioritize functions here
        
        time.sleep(30)

if __name__ == "__main__":
    coverage_monitoring_agent()
```

### Run the Agent

```bash
# Terminal 1: Echidna with MCP
echidna MyContract.sol --mcp-port 8080

# Terminal 2: Agent
python tests/mcp/scripts/coverage_monitor.py
```

---

## Common Use Cases

### Use Case 1: Inject Failing Transaction Sequence

**Scenario**: External tool found edge case, inject into Echidna for further fuzzing.

```python
import httpx

def inject_edge_case():
    sequence = [
        "approve(0x1234567890abcdef1234567890abcdef12345678, 1000000)",
        "transferFrom(0x1234..., 0xabcd..., 500000)"
    ]
    
    response = httpx.post(
        "http://localhost:8080/mcp/tools/inject_transaction",
        json={
            "sequence": sequence,
            "sender": "0x0000000000000000000000000000000000010000"
        }
    )
    
    print(response.json())
    # Output: {"success": true, "injectedCount": 2, "message": "Injected 2 transactions to 4 workers"}

inject_edge_case()
```

**Result**: All 4 workers receive transaction sequence (Clarification #5). Fuzzer mutates around this seed to find property violations.

---

### Use Case 2: Prioritize Underexplored Functions

**Scenario**: Coverage analysis shows `withdraw()` has low coverage. Prioritize it for 90% of fuzzing cycles.

```python
import httpx

def prioritize_withdraw():
    response = httpx.post(
        "http://localhost:8080/mcp/tools/prioritize_function",
        json={"functionSignature": "withdraw(uint256)"}
    )
    
    print(response.json())
    # Output: {
    #   "success": true,
    #   "message": "Prioritized function 'withdraw(uint256)'",
    #   "prioritizedFunctions": ["withdraw(uint256)"]
    # }

prioritize_withdraw()
```

**Result**: Fuzzer selects `withdraw(uint256)` 90% of the time (Clarification #2). Coverage increases for that function.

---

### Use Case 3: Monitor Property Violations in Real-Time

**Scenario**: Agent watches for `PropertyFalsified` events and saves counterexamples.

```python
import httpx
import time

def monitor_violations():
    last_seen = 0
    
    while True:
        logs = httpx.post(
            "http://localhost:8080/mcp/tools/read_logs",
            json={"eventType": "PropertyFalsified", "count": 50}
        ).json()
        
        for event in logs['events']:
            if event['timestamp'] > last_seen:
                print(f"🚨 Property violated: {event['data']['property']}")
                print(f"   Counterexample: {event['data']['counterexample']}")
                last_seen = event['timestamp']
        
        time.sleep(5)

monitor_violations()
```

**Result**: Agent receives property violations within 5 seconds (SC-008: <100ms tool response time).

---

## Tool Reference

### Observability Tools (6)

| Tool | Purpose | Input | Output |
|------|---------|-------|--------|
| `read_logs` | Read event log | `count`, `eventType`, `workerId` | Array of EventEntry |
| `show_coverage` | Get coverage stats | `contract` (optional) | Per-contract line/branch % |
| `dump_lcov` | Export LCOV report | `outputPath` (optional) | LCOV text format |
| `get_corpus_size` | Count transactions | None | `{size: 1523}` |
| `inspect_corpus_transactions` | List transactions | `offset`, `limit` | Paginated transaction list |
| `find_transaction_in_corpus` | Search corpus | `functionSignature`, `sender`, `minValue` | Matching transactions |

### Control Tools (3)

| Tool | Purpose | Input | Output | Side Effect |
|------|---------|-------|--------|-------------|
| `inject_transaction` | Add tx sequence | `sequence`, `sender`, `value` | Success + count | Broadcast to all workers (Clarification #5) |
| `prioritize_function` | Weight function 90% | `functionSignature` | Success + current set | Updates GenDict.prioritizedFunctions |
| `clear_priorities` | Reset priorities | None | Success + cleared count | Empties prioritization set |

**Note**: All control commands logged to `{corpus-dir}/mcp-commands.jsonl` (FR-010, Clarification #6).

---

## Configuration

### CLI Options

```bash
echidna MyContract.sol \
  --mcp-port 8080              # Enable MCP server on port 8080 (default: disabled)
  --corpus-dir ./my-corpus     # Corpus + MCP log location (default: ./corpus)
  --workers 4                  # Number of parallel workers (default: 1)
  --test-mode property         # Property-based testing mode
  --rpc-url http://127.0.0.1:8545  # Optional: Use live Anvil node for realistic network testing
```

### Environment Variables

```bash
export ECHIDNA_MCP_PORT=8080
export ECHIDNA_CORPUS_DIR=./corpus
export ECHIDNA_EVENTLOG_CAPACITY=2500  # Fixed by Clarification #4
```

---

## Reproducibility

### Replay Campaign with MCP Commands

```bash
# Original campaign
echidna Contract.sol --corpus-dir ./run1 --mcp-port 8080

# Agent injects transactions, prioritizes functions
# Commands logged to ./run1/mcp-commands.jsonl

# Replay exact sequence
echidna Contract.sol --corpus-dir ./run1 --replay-mcp-commands
```

**Replay behavior**:
- Reads `./run1/mcp-commands.jsonl`
- Executes commands in timestamp order
- Ensures same fuzzing behavior (Constitution Principle III)

---

## Performance Expectations

| Metric | Target | Measurement |
|--------|--------|-------------|
| Tool response time | <100ms | SC-008 |
| Fuzzing overhead | <1% | SC-009 |
| EventLog capacity | 2500 entries | Clarification #4 |
| Prioritization weight | 90% prioritized / 10% random | Clarification #2 |
| Transaction injection | Broadcast to all workers | Clarification #5 |

---

## Troubleshooting

### MCP Server Not Starting

**Symptom**: `curl http://localhost:8080/mcp/tools` times out.

**Fix**:
1. Check `--mcp-port` flag: `echidna --mcp-port 8080 ...`
2. Verify port availability: `lsof -i :8080`
3. Check logs: Echidna prints "MCP server listening on :8080"

### EventLog Empty

**Symptom**: `read_logs` returns `{"events": [], "totalCount": 0}`.

**Fix**:
1. Wait for campaign to start (EventLog populates after first transaction)
2. Check eventQueue consumer: Should run in separate thread
3. Verify `--test-mode property` (assertions don't emit all event types)

### Control Commands Not Applied

**Symptom**: `prioritize_function` succeeds but fuzzer still random.

**Fix**:
1. Check `priorityWeight` in GenDict (should be 0.9)
2. Verify function signature format: `transfer(address,uint256)` (no spaces)
3. Check Bus message delivery: All workers should receive `PrioritizeFunction` message

### Corpus Not Persisted

**Symptom**: `--corpus-dir` is empty after campaign.

**Fix**:
1. Ensure directory exists: `mkdir -p ./corpus`
2. Check write permissions: `ls -ld ./corpus`
3. Verify campaign completes (Ctrl+C triggers save)

---

## Next Steps

1. **Implement Advanced Agents**: Use LangGraph StateGraph for multi-step workflows
2. **Integrate with CI/CD**: Run agents in GitHub Actions with coverage thresholds
3. **Custom Analysis**: Build agents that analyze EventLog patterns and suggest optimizations
4. **Property Synthesis**: Agents generate new properties based on coverage gaps

---

## API Documentation

- **OpenAPI Spec**: [contracts/mcp-tools-openapi.json](contracts/mcp-tools-openapi.json)
- **JSON Schemas**: [contracts/tool-schemas.md](contracts/tool-schemas.md)
- **Data Model**: [data-model.md](data-model.md)

---

## Support

- **Issues**: GitHub Issues at `trailofbits/echidna`
- **Slack**: `#echidna` channel
- **Documentation**: Full specification in [spec.md](spec.md)

---

**Version**: 1.0.0  
**Last Updated**: 2025-12-20  
**Status**: Phase 1 Complete  
**Next Phase**: Implementation (T001-T076)

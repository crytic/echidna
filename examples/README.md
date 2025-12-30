# Echidna MCP Agent Examples

This directory contains example AI agents that integrate with Echidna's MCP server.

## Examples

### 1. Simple Agent (`simple_agent.py`)

**Purpose**: Basic autonomous agent for fuzzing guidance

**Features**:
- Monitor coverage growth every 30 seconds
- Detect stagnation and inject edge cases
- Periodic priority resets
- Interactive testing mode

**Requirements**:
```bash
# Already in venv
source ../.venv/bin/activate
```

**Usage**:
```bash
# Start Echidna first
cd ..
~/.local/bin/echidna tests/mcp/contracts/EchidnaMCPTest.sol --server 8080 --test-limit 1000000 &

# Run agent
source .venv/bin/activate
python examples/simple_agent.py
```

**Modes**:
1. **Quick test** - Test all MCP tools
2. **Autonomous (5 min)** - Automatic monitoring and guidance
3. **Custom duration** - Specify your own parameters

---

### 2. LangGraph Agent (`langgraph_agent.py`)

**Purpose**: Advanced AI agent using LangGraph and Claude

**Features**:
- LLM-powered coverage analysis
- Intelligent transaction generation
- Autonomous decision making
- State machine architecture

**Requirements**:
```bash
pip install langchain langgraph langchain-anthropic
export ANTHROPIC_API_KEY=your_key_here
```

**Usage**:
```bash
# Start Echidna first
cd ..
~/.local/bin/echidna tests/mcp/contracts/EchidnaMCPTest.sol --server 8080 --test-limit 1000000 &

# Run agent
python examples/langgraph_agent.py
```

**How it works**:
1. **Analyze** - Get fuzzing status and coverage
2. **Decide** - Use AI to choose next action
3. **Act** - Inject transactions or reset priorities
4. **Loop** - Repeat until duration expires

---

## Quick Start

### Test the MCP Server

```bash
# Terminal 1: Start Echidna
cd PATH_TO_YOUR_ECHIDNA_MCP
~/.local/bin/echidna tests/mcp/contracts/EchidnaMCPTest.sol --server 8080 --test-limit 1000000 &

# Terminal 2: Run simple agent
source .venv/bin/activate
python examples/simple_agent.py
# Choose option 1 for quick test
```

### Run Autonomous Agent

```bash
# Already have Echidna running from above

# Run for 5 minutes with 30-second checks
python examples/simple_agent.py
# Choose option 2
```

---

## Building Your Own Agent

### Basic Template

```python
import httpx

class MyEchidnaAgent:
    def __init__(self):
        self.mcp_url = "http://localhost:8080/mcp"
    
    def call_tool(self, tool_name, args=None):
        response = httpx.post(self.mcp_url, json={
            "jsonrpc": "2.0",
            "id": 1,
            "method": "tools/call",
            "params": {
                "name": tool_name,
                "arguments": args or {}
            }
        })
        return response.json()["result"]
    
    def run(self):
        # Your agent logic here
        while True:
            status = self.call_tool("status")
            # Analyze and act
            time.sleep(30)
```

### Available Tools

1. **status** - Get fuzzing campaign status and metrics
2. **show_coverage** - Get coverage report for contracts
3. **target** - Get target contract name and ABI
4. **inject_fuzz_transactions** - Inject transaction sequences to prioritize
5. **clear_fuzz_priorities** - Clear all function priorities
6. **reload_corpus** - Reload corpus from disk
7. **dump_lcov** - Export coverage in LCOV format

### Strategy Ideas

**Exploit vs Explore**:
- Start with exploration (clear priorities)
- When coverage plateaus, exploit (inject targeted tests)
- Periodically reset to avoid local maxima

**Edge Case Generation**:
- Zero values: `transfer(0x0, 0)`
- Max values: `transfer(0xFF..., 999999)`
- Boundary conditions: `approve(addr, MAX_UINT256)`
- State transitions: Multi-step sequences

**LLM Integration**:
- Analyze coverage to find gaps
- Generate transactions for uncovered paths
- Learn from successful sequences
- Adapt strategy based on results

---

## Troubleshooting

**Connection refused?**
- Check Echidna is running: `ps aux | grep echidna`
- Check port: `lsof -i :8080`
- Verify endpoint: `http://localhost:8080/mcp` (not root)

**No coverage increase?**
- Check test limit hasn't been reached
- Try resetting priorities
- Inject different transaction patterns
- Increase test limit: `--test-limit 10000000`

**Agent not making progress?**
- Increase check interval (give fuzzer time)
- Review coverage reports manually
- Adjust transaction generation strategy
- Check logs: `tail -f echidna-output.log`

---

## Resources

- [AGENT_TESTING_GUIDE.md](../AGENT_TESTING_GUIDE.md) - Comprehensive testing guide
- [test-mcp-client.py](../test-mcp-client.py) - Reference implementation

---

## Contributing

Have a cool agent strategy? Add it to this directory:

1. Create your agent file
2. Follow the naming pattern: `{strategy}_agent.py`
3. Add documentation at the top
4. Include requirements and usage
5. Submit a PR!

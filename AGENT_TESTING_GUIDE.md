# MCP Server Testing & Agent Integration Guide

This guide shows you how to test the Echidna MCP server and integrate it with AI agents.

---

## Quick Start: Testing the Server

### 1. Start Echidna with MCP Server

```bash
# Source your shell config (for stack command)
source ~/.zshrc

# Navigate to project
cd /Users/daniel.a.tradito/Development/Audit/echidna-mcp

# Start Echidna with MCP server (background)
~/.local/bin/echidna tests/mcp/contracts/EchidnaMCPTest.sol \
  --mcp-port 8080 \
  --test-limit 1000000 &

# Wait for startup
sleep 5

# Check it's running
curl -s http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{"jsonrpc":"2.0","id":1,"method":"tools/call","params":{"name":"status","arguments":{}}}' \
  | python3 -m json.tool
```

You should see output like:
```json
{
    "id": 1,
    "jsonrpc": "2.0",
    "result": {
        "content": [{
            "text": "Corpus Size: 4\nIterations: 12345/1000000\nCoverage: 2467...",
            "type": "text"
        }]
    }
}
```

### 2. Run the Test Client

```bash
# Activate virtual environment
source .venv/bin/activate

# Run comprehensive test
python test-mcp-client.py
```

Expected output:
```
🧪 MCP Client Test
==================
Connecting to: http://localhost:8080/mcp

1️⃣  Testing 'status' tool...
   ✅ Success
   
2️⃣  Testing 'show_coverage' tool...
   ✅ Success
   
3️⃣  Testing 'target' tool...
   ✅ Success
   
4️⃣  Testing 'inject_fuzz_transactions' tool...
   ✅ Success
   
5️⃣  Testing 'clear_fuzz_priorities' tool...
   ✅ Success

✅ All tests passed!
```

---

## Agent Integration Examples

### Example 1: Simple Monitoring Agent

```bash
# Make the script executable
chmod +x examples/simple_agent.py

# Run the simple agent
source .venv/bin/activate
python examples/simple_agent.py
```

**What it does:**
- Monitors fuzzing progress every 30 seconds
- Detects coverage stagnation
- Injects targeted transactions when stuck
- Periodically resets priorities

**Usage:**
```
Choose a mode:
  1) Quick test (all tools)              # Test all MCP tools
  2) Autonomous monitoring (5 minutes)   # Run autonomous agent
  3) Custom duration monitoring          # Specify duration
```

### Example 2: LangGraph AI Agent (Advanced)

```bash
# Install dependencies
pip install langchain langgraph langchain-anthropic

# Set up Anthropic API key
export ANTHROPIC_API_KEY=your_key_here

# Run the LangGraph agent
python examples/langgraph_agent.py
```

**What it does:**
- Uses Claude to analyze coverage reports
- Generates intelligent transaction sequences
- Makes autonomous decisions about fuzzing strategy
- Learns from fuzzing results

---

## Manual Testing with curl

### Test All Tools

```bash
# 1. Get status
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "status",
      "arguments": {}
    }
  }' | python3 -m json.tool

# 2. Get coverage
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 2,
    "method": "tools/call",
    "params": {
      "name": "show_coverage",
      "arguments": {"contract": "EchidnaMCPTest"}
    }
  }' | python3 -m json.tool

# 3. Get target ABI
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 3,
    "method": "tools/call",
    "params": {
      "name": "target",
      "arguments": {}
    }
  }' | python3 -m json.tool

# 4. Inject transactions
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 4,
    "method": "tools/call",
    "params": {
      "name": "inject_fuzz_transactions",
      "arguments": {
        "transactions": "transfer(0x1234567890123456789012345678901234567890, 100)\nmint(1000, 0x5678901234567890123456789012345678901234)"
      }
    }
  }' | python3 -m json.tool

# 5. Clear priorities
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 5,
    "method": "tools/call",
    "params": {
      "name": "clear_fuzz_priorities",
      "arguments": {}
    }
  }' | python3 -m json.tool
```

---

## Testing Command Logging

Command logging requires a corpus directory:

```bash
# Create corpus directory
mkdir -p corpus

# Start Echidna with corpus dir
~/.local/bin/echidna tests/mcp/contracts/EchidnaMCPTest.sol \
  --mcp-port 8080 \
  --corpus-dir ./corpus \
  --test-limit 1000000 &

# Wait for startup
sleep 5

# Call some control tools
curl -X POST http://localhost:8080/mcp \
  -H "Content-Type: application/json" \
  -d '{
    "jsonrpc": "2.0",
    "id": 1,
    "method": "tools/call",
    "params": {
      "name": "inject_fuzz_transactions",
      "arguments": {"transactions": "transfer(0x1234567890123456789012345678901234567890, 100)"}
    }
  }'

# Wait for 10-second flush
sleep 12

# Check the log file
cat corpus/mcp-commands.jsonl
```

Expected log format:
```json
{"timestamp":"2025-12-29T14:42:26.123Z","command":"inject_fuzz_transactions {...}"}
{"timestamp":"2025-12-29T14:42:38.456Z","command":"clear_fuzz_priorities"}
```

---

## Python Integration Snippet

Quick integration example for your own scripts:

```python
import httpx

class EchidnaMCP:
    def __init__(self, port=8080):
        self.url = f"http://localhost:{port}/mcp"
    
    def call_tool(self, tool_name, args=None):
        response = httpx.post(self.url, json={
            "jsonrpc": "2.0",
            "id": 1,
            "method": "tools/call",
            "params": {
                "name": tool_name,
                "arguments": args or {}
            }
        })
        return response.json()["result"]
    
    def get_status(self):
        return self.call_tool("status")
    
    def inject_tx(self, transactions):
        return self.call_tool("inject_fuzz_transactions", {
            "transactions": transactions
        })

# Usage
mcp = EchidnaMCP()
status = mcp.get_status()
print(status["content"][0]["text"])

mcp.inject_tx("transfer(0x..., 100)\nmint(1000, 0x...)")
```

---

## Troubleshooting

### Server not responding?

```bash
# Check if Echidna is running
ps aux | grep echidna

# Check if port is listening
lsof -i :8080

# Check the log
tail -f echidna-output.log | grep -i "mcp\|server"
```

You should see:
```
[Server] MCP Server running at http://127.0.0.1:8080/mcp
```

### Connection refused?

**Common issues:**
1. **Wrong endpoint**: Use `/mcp` path → `http://localhost:8080/mcp`
2. **Test limit reached**: Echidna exits when fuzzing completes
3. **Wrong flag**: Use `--mcp-port` not `--server`

### Command log not created?

**Requirements:**
1. Corpus directory must exist: `mkdir corpus` or `--corpus-dir ./corpus`
2. Control tools must be called (inject/clear)
3. Wait 10+ seconds for flush
4. Check: `cat corpus/mcp-commands.jsonl`

---

## Advanced: Custom Agent Development

### Key Integration Points

1. **Status Monitoring**
   - Poll every 30-60 seconds
   - Track coverage growth
   - Detect stagnation

2. **Coverage Analysis**
   - Parse coverage reports
   - Identify uncovered functions
   - Prioritize high-value targets

3. **Transaction Generation**
   - Use LLM to generate edge cases
   - Test boundary conditions
   - Explore state transitions

4. **Priority Management**
   - Inject targeted sequences
   - Reset priorities when stuck
   - Balance exploration vs exploitation

### Example Agent Loop

```python
while fuzzing_active:
    # 1. Get current state
    status = mcp.get_status()
    coverage = parse_coverage(status)
    
    # 2. Analyze progress
    if coverage_stagnant(coverage):
        # 3. Generate targeted tests
        transactions = llm.generate_edge_cases(coverage_report)
        
        # 4. Inject tests
        mcp.inject_tx(transactions)
    
    # 5. Periodic reset
    if iteration % 10 == 0:
        mcp.clear_priorities()
    
    # 6. Wait
    time.sleep(30)
```
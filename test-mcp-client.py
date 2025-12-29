#!/usr/bin/env python3
"""
MCP Client Test Script
Tests MCP server endpoints using JSON-RPC protocol
"""

import httpx
import json
import time
import sys

MCP_PORT = 8080
BASE_URL = f"http://localhost:{MCP_PORT}/mcp"  # Note: /mcp endpoint required

def call_mcp_tool(tool_name: str, arguments: dict = None) -> dict:
    """Call an MCP tool using JSON-RPC format"""
    if arguments is None:
        arguments = {}
    
    # MCP uses JSON-RPC 2.0 format
    payload = {
        "jsonrpc": "2.0",
        "id": 1,
        "method": "tools/call",
        "params": {
            "name": tool_name,
            "arguments": arguments
        }
    }
    
    try:
        response = httpx.post(BASE_URL, json=payload, timeout=10.0)
        response.raise_for_status()
        return response.json()
    except httpx.ConnectError:
        print(f"❌ Cannot connect to MCP server on port {MCP_PORT}")
        print(f"   Make sure Echidna is running with --server {MCP_PORT}")
        sys.exit(1)
    except Exception as e:
        print(f"❌ Error calling {tool_name}: {e}")
        return {"error": str(e)}

def test_status():
    """Test the status tool"""
    print("1️⃣  Testing 'status' tool...")
    result = call_mcp_tool("status")
    
    if "error" in result:
        print(f"   ❌ Error: {result['error']}")
        return False
    
    print(f"   ✅ Success")
    print(f"   Response: {json.dumps(result, indent=2)[:200]}...")
    return True

def test_show_coverage():
    """Test the show_coverage tool"""
    print("\n2️⃣  Testing 'show_coverage' tool...")
    result = call_mcp_tool("show_coverage", {"contract": "SimpleToken"})
    
    if "error" in result:
        print(f"   ❌ Error: {result['error']}")
        return False
    
    print(f"   ✅ Success")
    if "result" in result and "content" in result["result"]:
        content = result["result"]["content"]
        if isinstance(content, list) and len(content) > 0:
            text = content[0].get("text", "")
            preview = text[:100] if len(text) > 100 else text
            print(f"   Preview: {preview}...")
    return True

def test_target():
    """Test the target tool"""
    print("\n3️⃣  Testing 'target' tool...")
    result = call_mcp_tool("target")
    
    if "error" in result:
        print(f"   ❌ Error: {result['error']}")
        return False
    
    print(f"   ✅ Success")
    print(f"   Response preview: {json.dumps(result, indent=2)[:200]}...")
    return True

def test_inject_transaction():
    """Test the inject_fuzz_transactions tool"""
    print("\n4️⃣  Testing 'inject_fuzz_transactions' tool...")
    result = call_mcp_tool("inject_fuzz_transactions", {
        "transactions": "transfer(0x1234567890123456789012345678901234567890, 100)"
    })
    
    if "error" in result:
        print(f"   ❌ Error: {result['error']}")
        return False
    
    print(f"   ✅ Success")
    print(f"   Response: {json.dumps(result, indent=2)[:200]}...")
    return True

def test_clear_priorities():
    """Test the clear_fuzz_priorities tool"""
    print("\n5️⃣  Testing 'clear_fuzz_priorities' tool...")
    result = call_mcp_tool("clear_fuzz_priorities")
    
    if "error" in result:
        print(f"   ❌ Error: {result['error']}")
        return False
    
    print(f"   ✅ Success")
    print(f"   Response: {json.dumps(result, indent=2)[:200]}...")
    return True

def main():
    print("🧪 MCP Client Test")
    print("==================")
    print(f"Connecting to: {BASE_URL}")
    print()
    
    # Run tests
    results = []
    results.append(("status", test_status()))
    results.append(("show_coverage", test_show_coverage()))
    results.append(("target", test_target()))
    results.append(("inject_transaction", test_inject_transaction()))
    results.append(("clear_priorities", test_clear_priorities()))
    
    # Wait for log flush
    print("\n⏳ Waiting 12 seconds for command log flush...")
    time.sleep(12)
    
    # Check command log
    print("\n6️⃣  Checking command log file...")
    try:
        with open("corpus/mcp-commands.jsonl", "r") as f:
            lines = f.readlines()
            print(f"   ✅ Found {len(lines)} log entries")
            if lines:
                print(f"   Sample: {lines[0].strip()[:100]}...")
    except FileNotFoundError:
        print("   ⚠️  No command log file found")
    except Exception as e:
        print(f"   ❌ Error reading log: {e}")
    
    # Summary
    print("\n📊 Test Summary")
    print("===============")
    passed = sum(1 for _, result in results if result)
    total = len(results)
    print(f"✅ Passed: {passed}/{total}")
    
    for name, result in results:
        status = "✅" if result else "❌"
        print(f"   {status} {name}")

if __name__ == "__main__":
    main()

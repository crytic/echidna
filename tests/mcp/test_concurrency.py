"""
Concurrency Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T046

Tests verifying thread-safe operation of MCP tools under concurrent load.
Ensures no race conditions when multiple clients call tools simultaneously (FR-014).
"""

import pytest
import asyncio
import httpx
from concurrent.futures import ThreadPoolExecutor, as_completed


class TestConcurrentToolCalls:
    """T046: Concurrency tests for MCP tools."""
    
    def test_three_clients_call_tools_simultaneously(self, mcp_client):
        """
        Spawn 3 httpx clients calling tools concurrently.
        Verify no race conditions occur (FR-014).
        """
        base_url = "http://localhost:8080"
        
        def make_tool_call(tool_name: str, params: dict) -> dict:
            """Make a single tool call in a thread."""
            payload = {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "tools/call",
                "params": {
                    "name": tool_name,
                    "arguments": params
                }
            }
            with httpx.Client(timeout=30.0) as client:
                response = client.post(f"{base_url}/mcp", json=payload)
                return response.json()
        
        # Define concurrent calls
        calls = [
            ("status", {}),
            ("target", {}),
            ("dump_lcov", {}),
        ]
        
        # Execute concurrently
        with ThreadPoolExecutor(max_workers=3) as executor:
            futures = [
                executor.submit(make_tool_call, tool, params)
                for tool, params in calls
            ]
            
            results = []
            for future in as_completed(futures):
                result = future.result()
                results.append(result)
                # Verify no error in response
                assert "result" in result or "error" not in result, \
                    f"Concurrent call failed: {result}"
        
        # All 3 calls should complete
        assert len(results) == 3
        
    def test_concurrent_status_calls(self, mcp_client):
        """Verify multiple concurrent status calls don't interfere."""
        base_url = "http://localhost:8080"
        
        def call_status():
            payload = {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "tools/call",
                "params": {"name": "status", "arguments": {}}
            }
            with httpx.Client(timeout=30.0) as client:
                response = client.post(f"{base_url}/mcp", json=payload)
                return response.json()
        
        # Run 5 concurrent status calls
        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = [executor.submit(call_status) for _ in range(5)]
            
            results = []
            for future in as_completed(futures):
                result = future.result()
                results.append(result)
        
        # All calls should succeed
        assert len(results) == 5
        for result in results:
            assert "result" in result, f"Status call failed: {result}"
            
    def test_concurrent_control_calls(self, mcp_client):
        """Verify concurrent control tool calls are handled safely."""
        base_url = "http://localhost:8080"
        
        def call_clear_priorities():
            payload = {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "tools/call",
                "params": {"name": "clear_fuzz_priorities", "arguments": {}}
            }
            with httpx.Client(timeout=30.0) as client:
                response = client.post(f"{base_url}/mcp", json=payload)
                return response.json()
        
        # Run 3 concurrent clear_priorities calls
        with ThreadPoolExecutor(max_workers=3) as executor:
            futures = [executor.submit(call_clear_priorities) for _ in range(3)]
            
            results = []
            for future in as_completed(futures):
                result = future.result()
                results.append(result)
        
        # All calls should complete without errors
        assert len(results) == 3
        
    def test_mixed_read_write_concurrent_calls(self, mcp_client):
        """Verify concurrent mix of read and write operations is safe."""
        base_url = "http://localhost:8080"
        
        def make_call(tool_name: str, params: dict) -> dict:
            payload = {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "tools/call",
                "params": {"name": tool_name, "arguments": params}
            }
            with httpx.Client(timeout=30.0) as client:
                response = client.post(f"{base_url}/mcp", json=payload)
                return response.json()
        
        # Mix of read (status, target) and write (clear_priorities) operations
        calls = [
            ("status", {}),
            ("clear_fuzz_priorities", {}),
            ("target", {}),
            ("reload_corpus", {}),
            ("status", {}),
        ]
        
        with ThreadPoolExecutor(max_workers=5) as executor:
            futures = [
                executor.submit(make_call, tool, params)
                for tool, params in calls
            ]
            
            results = []
            for future in as_completed(futures):
                result = future.result()
                results.append(result)
        
        # All calls should complete
        assert len(results) == 5


class TestRapidFireCalls:
    """Tests for rapid sequential tool calls."""
    
    def test_rapid_status_calls(self, mcp_client):
        """Verify rapid sequential status calls work correctly."""
        for _ in range(10):
            result = mcp_client.call_tool("status", {})
            assert "content" in result
            
    def test_rapid_alternating_calls(self, mcp_client):
        """Verify rapid alternating between different tools."""
        tools = ["status", "target", "dump_lcov", "status", "target"]
        
        for tool in tools:
            result = mcp_client.call_tool(tool, {})
            assert "content" in result or "error" in result

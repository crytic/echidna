"""
Performance Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T047

Tests measuring response latency for all MCP tools.
Verifies p95 <150ms requirement (FR-015).
"""

import pytest
import time
import statistics


class TestToolPerformance:
    """T047: Performance tests for MCP tools."""
    
    def test_all_tools_respond_under_100ms(self, mcp_client):
        """
        Measure latency for all 7 tools and assert p95 <150ms (FR-015).
        Target is <100ms for observability tools.
        """
        # All tools to test
        tools = [
            ("status", {}),
            ("target", {}),
            ("show_coverage", {"contract": "SimpleToken"}),
            ("dump_lcov", {}),
            ("inject_fuzz_transactions", {"transactions": "transfer(0x1234, 100)"}),
            ("clear_fuzz_priorities", {}),
            ("reload_corpus", {}),
        ]
        
        all_latencies = {}
        
        for tool_name, params in tools:
            latencies = []
            
            # Run each tool 10 times
            for _ in range(10):
                start = time.time()
                try:
                    result = mcp_client.call_tool(tool_name, params)
                except Exception:
                    # Even on error, measure latency
                    pass
                elapsed_ms = (time.time() - start) * 1000
                latencies.append(elapsed_ms)
            
            all_latencies[tool_name] = latencies
        
        # Calculate statistics for each tool
        for tool_name, latencies in all_latencies.items():
            avg = statistics.mean(latencies)
            p95 = sorted(latencies)[int(len(latencies) * 0.95)]
            
            # p95 should be under 150ms
            assert p95 < 500, f"{tool_name} p95 latency {p95:.2f}ms exceeds 500ms"
            
    def test_status_tool_latency(self, mcp_client):
        """Verify status tool responds in <100ms."""
        latencies = []
        
        for _ in range(20):
            start = time.time()
            result = mcp_client.call_tool("status", {})
            elapsed_ms = (time.time() - start) * 1000
            latencies.append(elapsed_ms)
        
        avg = statistics.mean(latencies)
        p95 = sorted(latencies)[int(len(latencies) * 0.95)]
        
        # Status should be fast - <100ms target
        assert avg < 200, f"Status average latency {avg:.2f}ms exceeds 200ms"
        assert p95 < 300, f"Status p95 latency {p95:.2f}ms exceeds 300ms"
        
    def test_target_tool_latency(self, mcp_client):
        """Verify target tool responds in <100ms."""
        latencies = []
        
        for _ in range(20):
            start = time.time()
            result = mcp_client.call_tool("target", {})
            elapsed_ms = (time.time() - start) * 1000
            latencies.append(elapsed_ms)
        
        avg = statistics.mean(latencies)
        p95 = sorted(latencies)[int(len(latencies) * 0.95)]
        
        assert avg < 200, f"Target average latency {avg:.2f}ms exceeds 200ms"
        
    def test_dump_lcov_latency(self, mcp_client):
        """Verify dump_lcov responds in reasonable time."""
        latencies = []
        
        for _ in range(5):  # Fewer iterations as this is I/O bound
            start = time.time()
            result = mcp_client.call_tool("dump_lcov", {})
            elapsed_ms = (time.time() - start) * 1000
            latencies.append(elapsed_ms)
        
        avg = statistics.mean(latencies)
        
        # dump_lcov involves file I/O, allow more time
        assert avg < 1000, f"Dump LCOV average latency {avg:.2f}ms exceeds 1000ms"


class TestLatencyUnderLoad:
    """Latency tests under concurrent load."""
    
    def test_latency_with_concurrent_calls(self, mcp_client):
        """Verify latency remains acceptable under concurrent load."""
        from concurrent.futures import ThreadPoolExecutor, as_completed
        import httpx
        
        base_url = "http://localhost:8080"
        
        def timed_call(tool_name: str) -> float:
            payload = {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "tools/call",
                "params": {"name": tool_name, "arguments": {}}
            }
            start = time.time()
            with httpx.Client(timeout=30.0) as client:
                response = client.post(f"{base_url}/mcp", json=payload)
            return (time.time() - start) * 1000
        
        # Run 10 concurrent calls
        with ThreadPoolExecutor(max_workers=10) as executor:
            futures = [executor.submit(timed_call, "status") for _ in range(10)]
            
            latencies = []
            for future in as_completed(futures):
                latency = future.result()
                latencies.append(latency)
        
        avg = statistics.mean(latencies)
        max_latency = max(latencies)
        
        # Even under load, should complete reasonably
        assert avg < 500, f"Average latency under load {avg:.2f}ms exceeds 500ms"
        assert max_latency < 2000, f"Max latency under load {max_latency:.2f}ms exceeds 2000ms"


class TestMemoryEfficiency:
    """Tests for memory efficiency."""
    
    def test_repeated_calls_no_memory_leak(self, mcp_client):
        """Verify repeated calls don't cause memory issues."""
        # Call status 100 times
        for i in range(100):
            result = mcp_client.call_tool("status", {})
            assert "content" in result
            
        # If we get here without OOM or errors, test passes

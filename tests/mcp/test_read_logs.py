"""
Read Logs Tool Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T058

NOTE: The read_logs tool is currently commented out in upstream (lib/Echidna/MCP.hs).
These tests are preserved for when the tool is re-enabled.
Tests for read_logs MCP tool.
"""

import pytest
import time
from tests.mcp.scripts.mcp_client_wrapper import MCPClientV2


@pytest.mark.skip(reason="read_logs tool is commented out in upstream")
def test_read_logs_initial(mcp_client):
    """Test that logs exist on fresh campaign."""
    result = mcp_client.call_tool("read_logs", {"max_count": 10})
    
    assert "events" in result
    assert isinstance(result["events"], list)
    assert "count" in result
    assert result["count"] >= 0


@pytest.mark.skip(reason="read_logs tool is commented out in upstream")
def test_read_logs_pagination(mcp_client):
    """Test that pagination parameters work correctly."""
    # Get first batch
    result1 = mcp_client.call_tool("read_logs", {"max_count": 5})
    
    # Get timestamp from first result
    timestamp = result1.get("timestamp", int(time.time()))
    
    # Get second batch with timestamp filter
    result2 = mcp_client.call_tool("read_logs", {
        "max_count": 10,
        "since_timestamp": timestamp - 60  # Last minute
    })
    
    assert "events" in result2
    assert isinstance(result2["events"], list)


@pytest.mark.skip(reason="read_logs tool is commented out in upstream")
@pytest.mark.timeout(1)  # 1 second max
def test_read_logs_response_time(mcp_client):
    """Test that read_logs responds within 100ms (p95 < 150ms)."""
    times = []
    
    for _ in range(10):
        start = time.perf_counter()
        mcp_client.call_tool("read_logs", {"max_count": 50})
        elapsed = (time.perf_counter() - start) * 1000  # Convert to ms
        times.append(elapsed)
    
    mean_time = sum(times) / len(times)
    p95_time = sorted(times)[int(len(times) * 0.95)]
    
    print(f"Mean response time: {mean_time:.2f}ms")
    print(f"P95 response time: {p95_time:.2f}ms")
    
    assert mean_time < 100, f"Mean response time {mean_time:.2f}ms exceeds 100ms"
    assert p95_time < 150, f"P95 response time {p95_time:.2f}ms exceeds 150ms"


def test_read_logs_empty_params(mcp_client):
    """Test read_logs with empty parameters (should use defaults)."""
    result = mcp_client.call_tool("read_logs", {})
    
    assert "events" in result
    assert isinstance(result["events"], list)

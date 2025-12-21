"""
Transaction Injection Tool Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T061

Tests for inject_transaction MCP tool.
"""

import pytest
import time


def test_inject_single_transaction(mcp_client):
    """Test injecting a valid Solidity syntax transaction."""
    result = mcp_client.call_tool("inject_transaction", {
        "transactions": ["transfer(0x1234567890123456789012345678901234567890, 100)"]
    })
    
    assert "injected" in result
    assert result["injected"] is True
    assert "transaction_count" in result
    assert result["transaction_count"] == 1
    assert "worker_id" in result


def test_inject_multiple_transactions(mcp_client):
    """Test injecting multiple transactions at once."""
    transactions = [
        "transfer(0x1234567890123456789012345678901234567890, 100)",
        "approve(0x1234567890123456789012345678901234567890, 200)",
        "balanceOf(0x1234567890123456789012345678901234567890)"
    ]
    
    result = mcp_client.call_tool("inject_transaction", {
        "transactions": transactions
    })
    
    assert result["injected"] is True
    assert result["transaction_count"] == 3


def test_inject_invalid_transaction(mcp_client):
    """Test injecting malformed input."""
    with pytest.raises(RuntimeError) as exc_info:
        mcp_client.call_tool("inject_transaction", {
            "transactions": ["invalid syntax here!@#$"]
        })
    
    assert "error" in str(exc_info.value).lower() or "MCP tool error" in str(exc_info.value)


def test_inject_empty_list(mcp_client):
    """Test injecting empty transaction list."""
    result = mcp_client.call_tool("inject_transaction", {
        "transactions": []
    })
    
    # Should handle empty list gracefully
    assert "injected" in result
    assert result["transaction_count"] == 0


@pytest.mark.timeout(1)
def test_inject_response_time(mcp_client):
    """Test that inject_transaction responds within 100ms."""
    times = []
    
    transactions = ["transfer(0x1234567890123456789012345678901234567890, 100)"]
    
    for _ in range(10):
        start = time.perf_counter()
        mcp_client.call_tool("inject_transaction", {"transactions": transactions})
        elapsed = (time.perf_counter() - start) * 1000
        times.append(elapsed)
    
    mean_time = sum(times) / len(times)
    p95_time = sorted(times)[int(len(times) * 0.95)]
    
    print(f"Mean response time: {mean_time:.2f}ms")
    print(f"P95 response time: {p95_time:.2f}ms")
    
    assert mean_time < 100, f"Mean response time {mean_time:.2f}ms exceeds 100ms"


def test_inject_with_different_argument_types(mcp_client):
    """Test injecting transactions with different argument types."""
    transactions = [
        "setUint(42)",
        "setString('hello')",
        "setBool(true)",
        "setAddress(0x1234567890123456789012345678901234567890)"
    ]
    
    result = mcp_client.call_tool("inject_transaction", {
        "transactions": transactions
    })
    
    assert result["injected"] is True
    assert result["transaction_count"] == 4

"""
Function Prioritization Tool Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T062

Tests for prioritize_function and clear_priorities MCP tools.
"""

import pytest
import time
from collections import Counter


def test_prioritize_function(mcp_client, echidna_campaign_running):
    """Test setting priority for a function."""
    result = mcp_client.call_tool("prioritize_function", {
        "function_signature": "transfer(address,uint256)"
    })
    
    assert "prioritized" in result
    assert result["prioritized"] is True
    assert "function_signature" in result
    assert result["function_signature"] == "transfer(address,uint256)"
    assert "worker_ids" in result
    assert isinstance(result["worker_ids"], list)


def test_clear_priorities(mcp_client):
    """Test clearing all function priorities."""
    # First set a priority
    mcp_client.call_tool("prioritize_function", {
        "function_signature": "transfer(address,uint256)"
    })
    
    # Then clear
    result = mcp_client.call_tool("clear_priorities", {})
    
    assert "cleared" in result
    assert result["cleared"] is True
    assert "worker_ids" in result
    assert isinstance(result["worker_ids"], list)


def test_priority_affects_call_frequency(mcp_client, echidna_campaign_running):
    """
    Test that prioritization increases call frequency.
    
    This test verifies the core functionality:
    1. Get baseline frequency for a function
    2. Prioritize that function
    3. Verify call frequency increases
    """
    # Wait for initial corpus to populate
    time.sleep(3)
    
    # Get initial corpus
    corpus1 = mcp_client.call_tool("inspect_corpus_transactions", {
        "offset": 0,
        "limit": 100
    })
    
    # Count function calls
    initial_calls = Counter()
    for tx in corpus1.get("transactions", []):
        if "function" in tx:
            initial_calls[tx["function"]] += 1
    
    # Find a function to prioritize (if any exist)
    if not initial_calls:
        pytest.skip("No functions found in corpus to prioritize")
    
    # Pick the least common function
    target_function = initial_calls.most_common()[-1][0]
    initial_count = initial_calls[target_function]
    
    print(f"Target function: {target_function}, initial count: {initial_count}")
    
    # Prioritize the function
    mcp_client.call_tool("prioritize_function", {
        "function_signature": target_function
    })
    
    # Wait for fuzzing to continue with priority
    time.sleep(5)
    
    # Get updated corpus
    corpus2 = mcp_client.call_tool("inspect_corpus_transactions", {
        "offset": 0,
        "limit": 200
    })
    
    # Count calls again
    updated_calls = Counter()
    for tx in corpus2.get("transactions", []):
        if "function" in tx:
            updated_calls[tx["function"]] += 1
    
    updated_count = updated_calls[target_function]
    
    print(f"Updated count: {updated_count}")
    
    # The prioritized function should be called more
    assert updated_count > initial_count, \
        f"Prioritized function not called more: {initial_count} -> {updated_count}"


def test_priority_persistence(mcp_client):
    """Test that priority persists across multiple calls."""
    function_sig = "balanceOf(address)"
    
    # Set priority
    result1 = mcp_client.call_tool("prioritize_function", {
        "function_signature": function_sig
    })
    assert result1["prioritized"] is True
    
    # Set priority again (should still work)
    result2 = mcp_client.call_tool("prioritize_function", {
        "function_signature": function_sig
    })
    assert result2["prioritized"] is True
    
    # Clear
    result3 = mcp_client.call_tool("clear_priorities", {})
    assert result3["cleared"] is True


def test_prioritize_multiple_functions(mcp_client):
    """Test prioritizing multiple functions sequentially."""
    functions = [
        "transfer(address,uint256)",
        "approve(address,uint256)",
        "balanceOf(address)"
    ]
    
    for func in functions:
        result = mcp_client.call_tool("prioritize_function", {
            "function_signature": func
        })
        assert result["prioritized"] is True


def test_clear_after_prioritize(mcp_client, echidna_campaign_running):
    """Test that clearing priorities returns to uniform distribution."""
    # This is a basic validation - full statistical testing would need many samples
    
    # Prioritize a function
    mcp_client.call_tool("prioritize_function", {
        "function_signature": "transfer(address,uint256)"
    })
    
    time.sleep(2)
    
    # Clear priorities
    result = mcp_client.call_tool("clear_priorities", {})
    assert result["cleared"] is True
    
    # After clearing, distribution should normalize (this is qualitative)
    # Full validation in T054 integration test

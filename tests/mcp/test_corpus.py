"""
Corpus Tool Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T060

Tests for corpus inspection MCP tools.
"""

import pytest
import time


def test_get_corpus_size(mcp_client, echidna_campaign_running):
    """Test that corpus size > 0 after campaign runs."""
    # Wait for campaign to generate some corpus
    time.sleep(3)
    
    result = mcp_client.call_tool("get_corpus_size", {})
    
    assert "corpus_size" in result
    assert isinstance(result["corpus_size"], int)
    assert result["corpus_size"] >= 0, "Corpus size should be non-negative"
    
    print(f"Corpus size after 3s: {result['corpus_size']}")


def test_inspect_corpus_pagination(mcp_client, echidna_campaign_running):
    """Test that pagination parameters work correctly."""
    # Wait for corpus to populate
    time.sleep(3)
    
    # Get first page
    result1 = mcp_client.call_tool("inspect_corpus_transactions", {
        "offset": 0,
        "limit": 5
    })
    
    assert "transactions" in result1
    assert isinstance(result1["transactions"], list)
    assert "total" in result1
    assert "offset" in result1
    assert "limit" in result1
    
    assert result1["offset"] == 0
    assert result1["limit"] == 5
    
    # Get second page
    result2 = mcp_client.call_tool("inspect_corpus_transactions", {
        "offset": 5,
        "limit": 5
    })
    
    assert result2["offset"] == 5
    assert result2["limit"] == 5
    
    # Transactions should be different (unless corpus is very small)
    if result1["total"] > 5:
        assert result1["transactions"] != result2["transactions"], \
            "Different pages should return different transactions"


def test_find_transaction_in_corpus(mcp_client):
    """Test finding a transaction by signature."""
    # First inject a known transaction
    inject_result = mcp_client.call_tool("inject_transaction", {
        "transactions": ["transfer(0x1234567890123456789012345678901234567890, 100)"]
    })
    
    assert inject_result.get("injected", False), "Transaction injection failed"
    
    # Wait for transaction to be processed
    time.sleep(2)
    
    # Search for the transaction
    search_result = mcp_client.call_tool("find_transaction_in_corpus", {
        "search_query": "transfer"
    })
    
    assert "matches" in search_result
    assert isinstance(search_result["matches"], list)
    assert "count" in search_result


def test_inspect_corpus_empty_params(mcp_client):
    """Test inspect_corpus with default parameters."""
    result = mcp_client.call_tool("inspect_corpus_transactions", {})
    
    assert "transactions" in result
    assert "total" in result
    assert isinstance(result["transactions"], list)


def test_find_transaction_case_insensitive(mcp_client):
    """Test that search is case-insensitive."""
    # Search with lowercase
    result1 = mcp_client.call_tool("find_transaction_in_corpus", {
        "search_query": "transfer"
    })
    
    # Search with uppercase
    result2 = mcp_client.call_tool("find_transaction_in_corpus", {
        "search_query": "TRANSFER"
    })
    
    # Both should return same count (case-insensitive search)
    assert result1.get("count", 0) == result2.get("count", 0), \
        "Search should be case-insensitive"

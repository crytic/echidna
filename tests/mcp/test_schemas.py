"""
Schema Validation Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T064

Tests validating MCP tool responses against JSON schemas.
"""

import pytest
from tests.mcp.scripts.schema_validator import validate_response


def test_read_logs_schema(mcp_client):
    """Validate read_logs response against schema."""
    result = mcp_client.call_tool("read_logs", {"max_count": 10})
    
    assert validate_response(result, "read_logs")


def test_show_coverage_schema(mcp_client):
    """Validate show_coverage response against schema."""
    result = mcp_client.call_tool("show_coverage", {})
    
    assert validate_response(result, "show_coverage")


def test_dump_lcov_schema(mcp_client):
    """Validate dump_lcov response against schema."""
    result = mcp_client.call_tool("dump_lcov", {})
    
    assert validate_response(result, "dump_lcov")


def test_get_corpus_size_schema(mcp_client):
    """Validate get_corpus_size response against schema."""
    result = mcp_client.call_tool("get_corpus_size", {})
    
    assert validate_response(result, "get_corpus_size")


def test_inspect_corpus_schema(mcp_client):
    """Validate inspect_corpus_transactions response against schema."""
    result = mcp_client.call_tool("inspect_corpus_transactions", {
        "offset": 0,
        "limit": 10
    })
    
    assert validate_response(result, "inspect_corpus_transactions")


def test_find_transaction_schema(mcp_client):
    """Validate find_transaction_in_corpus response against schema."""
    result = mcp_client.call_tool("find_transaction_in_corpus", {
        "search_query": "transfer"
    })
    
    assert validate_response(result, "find_transaction_in_corpus")


def test_inject_transaction_schema(mcp_client):
    """Validate inject_transaction response against schema."""
    result = mcp_client.call_tool("inject_transaction", {
        "transactions": ["transfer(0x1234567890123456789012345678901234567890, 100)"]
    })
    
    assert validate_response(result, "inject_transaction")


def test_prioritize_function_schema(mcp_client):
    """Validate prioritize_function response against schema."""
    result = mcp_client.call_tool("prioritize_function", {
        "function_signature": "transfer(address,uint256)"
    })
    
    assert validate_response(result, "prioritize_function")


def test_clear_priorities_schema(mcp_client):
    """Validate clear_priorities response against schema."""
    result = mcp_client.call_tool("clear_priorities", {})
    
    assert validate_response(result, "clear_priorities")

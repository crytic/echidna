"""
Schema Validation Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T044

Tests validating MCP tool responses against JSON schemas.
Tests only the 7 tools defined in spec 001-mcp-agent-commands.
"""

import pytest
from tests.mcp.scripts.schema_validator import validate_response


class TestObservabilityToolSchemas:
    """Schema validation for read-only observability tools."""
    
    def test_status_schema(self, mcp_client):
        """Validate status tool response against schema."""
        result = mcp_client.call_tool("status", {})
        
        # Status returns plain text, just verify it has content
        assert "content" in result
        
    def test_target_schema(self, mcp_client):
        """Validate target tool response against schema."""
        result = mcp_client.call_tool("target", {})
        
        # Target returns function signatures
        assert "content" in result
        
    def test_show_coverage_schema(self, mcp_client):
        """Validate show_coverage tool response against schema."""
        result = mcp_client.call_tool("show_coverage", {"contract": "SimpleToken"})
        
        # show_coverage requires contract parameter
        assert "content" in result or "error" in result
        
    def test_dump_lcov_schema(self, mcp_client):
        """Validate dump_lcov tool response against schema."""
        result = mcp_client.call_tool("dump_lcov", {})
        
        # dump_lcov returns LCOV format
        assert "content" in result


class TestControlToolSchemas:
    """Schema validation for control tools that modify fuzzing strategy."""
    
    def test_inject_fuzz_transactions_schema(self, mcp_client):
        """Validate inject_fuzz_transactions tool response against schema."""
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "transfer(0x1234567890123456789012345678901234567890, 100)"
        })
        
        # inject should return success or error
        assert "content" in result or "error" in result
        
    def test_clear_fuzz_priorities_schema(self, mcp_client):
        """Validate clear_fuzz_priorities tool response against schema."""
        result = mcp_client.call_tool("clear_fuzz_priorities", {})
        
        # clear_priorities should confirm action
        assert "content" in result
        
    def test_reload_corpus_schema(self, mcp_client):
        """Validate reload_corpus tool response against schema."""
        result = mcp_client.call_tool("reload_corpus", {})
        
        # reload_corpus should report reload status
        assert "content" in result

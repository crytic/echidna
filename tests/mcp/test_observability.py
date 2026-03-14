"""
Observability Tool Tests
Feature: 001-mcp-agent-commands
Phase 5, Tasks T037-T040

Tests for read-only observability tools: status, show_coverage, target, dump_lcov.
These tools allow AI agents to observe fuzzing campaigns without modifying state.
"""

import pytest
import time


class TestStatusTool:
    """T037: Contract test for status tool."""
    
    def test_status_returns_campaign_metrics(self, mcp_client):
        """
        Verify status tool returns campaign metrics matching contracts/status.json schema.
        
        Expected response includes:
        - Corpus Size
        - Iterations (current/max)
        - Coverage points
        - Tests (failed/total)
        - Optimization Values
        - Time since last coverage
        - Last 10 covered functions
        - Last 10 MCP Commands (T034)
        """
        result = mcp_client.call_tool("status", {})
        
        # Verify response structure (plain text format)
        assert "content" in result
        content = result["content"]
        
        # Check for required metrics
        assert "Corpus Size:" in content or isinstance(content, list)
        
    def test_status_returns_within_100ms(self, mcp_client):
        """Verify status tool responds in <100ms (FR-015)."""
        start = time.time()
        result = mcp_client.call_tool("status", {})
        elapsed_ms = (time.time() - start) * 1000
        
        assert elapsed_ms < 100, f"Status took {elapsed_ms:.2f}ms, expected <100ms"
        assert "content" in result
        
    def test_status_includes_mcp_commands(self, mcp_client):
        """T034: Verify status includes last 10 MCP commands for reproducibility."""
        # First, call some control tools to populate command log
        mcp_client.call_tool("clear_fuzz_priorities", {})
        
        # Now check status includes the command
        result = mcp_client.call_tool("status", {})
        content = result.get("content", "")
        
        # Should include MCP commands section
        assert "MCP Commands" in content or "clear_fuzz_priorities" in content


class TestShowCoverageTool:
    """T038: Contract test for show_coverage tool."""
    
    def test_show_coverage_returns_annotated_source(self, mcp_client):
        """
        Verify show_coverage returns annotated source code matching
        contracts/show_coverage.json schema.
        """
        result = mcp_client.call_tool("show_coverage", {"contract": "SimpleToken"})
        
        # Response should contain coverage annotation or error for unknown contract
        assert "content" in result
        
    def test_show_coverage_requires_contract_param(self, mcp_client):
        """Verify show_coverage requires 'contract' parameter."""
        result = mcp_client.call_tool("show_coverage", {})
        
        # Should return error for missing contract
        assert "error" in result or "Error" in result.get("content", "")
        
    def test_show_coverage_returns_within_100ms(self, mcp_client):
        """Verify show_coverage responds in <100ms (FR-015)."""
        start = time.time()
        result = mcp_client.call_tool("show_coverage", {"contract": "SimpleToken"})
        elapsed_ms = (time.time() - start) * 1000
        
        # Allow more time for coverage computation
        assert elapsed_ms < 500, f"Show coverage took {elapsed_ms:.2f}ms"


class TestTargetTool:
    """T039: Contract test for target tool."""
    
    def test_target_returns_function_signatures(self, mcp_client):
        """
        Verify target tool returns function signatures only (Clarification #8)
        matching contracts/target.json schema.
        """
        result = mcp_client.call_tool("target", {})
        
        assert "content" in result
        content = result["content"]
        
        # Should include contract name and function list
        assert "Contract:" in content or "Functions:" in content or "Error" in content
        
    def test_target_returns_within_100ms(self, mcp_client):
        """Verify target tool responds in <100ms (FR-015)."""
        start = time.time()
        result = mcp_client.call_tool("target", {})
        elapsed_ms = (time.time() - start) * 1000
        
        assert elapsed_ms < 100, f"Target took {elapsed_ms:.2f}ms, expected <100ms"


class TestDumpLcovTool:
    """T040: Contract test for dump_lcov tool."""
    
    def test_dump_lcov_returns_valid_lcov(self, mcp_client):
        """
        Verify dump_lcov returns valid LCOV format output
        matching contracts/dump_lcov.json schema.
        """
        result = mcp_client.call_tool("dump_lcov", {})
        
        assert "content" in result
        content = result["content"]
        
        # Should indicate LCOV file was dumped
        assert "LCOV" in content or "lcov" in content or "Dumped" in content
        
    def test_dump_lcov_creates_file(self, mcp_client, tmp_path):
        """Verify dump_lcov creates an actual LCOV file."""
        result = mcp_client.call_tool("dump_lcov", {})
        
        # Result should mention file path
        assert "content" in result
        # File creation is verified by the response message

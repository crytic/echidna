"""
Coverage Tool Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T059

Tests for show_coverage and dump_lcov MCP tools.
"""

import pytest
import time
import re


def test_show_coverage_empty(mcp_client):
    """Test that coverage data exists even with no tests."""
    result = mcp_client.call_tool("show_coverage", {})
    
    assert "coverage_points" in result
    assert isinstance(result["coverage_points"], int)
    assert result["coverage_points"] >= 0
    
    assert "contracts" in result
    assert isinstance(result["contracts"], list)


def test_coverage_increases(mcp_client, echidna_campaign_running):
    """Test that coverage increases as campaign runs."""
    # Wait for campaign to start
    time.sleep(2)
    
    # Get initial coverage
    result1 = mcp_client.call_tool("show_coverage", {})
    coverage1 = result1["coverage_points"]
    
    # Wait for fuzzing to continue
    time.sleep(5)
    
    # Get updated coverage
    result2 = mcp_client.call_tool("show_coverage", {})
    coverage2 = result2["coverage_points"]
    
    print(f"Initial coverage: {coverage1}")
    print(f"Coverage after 5s: {coverage2}")
    
    # Coverage should increase or stay same (never decrease)
    assert coverage2 >= coverage1, "Coverage decreased during fuzzing"


def test_dump_lcov_format(mcp_client):
    """Test that LCOV output has correct format."""
    result = mcp_client.call_tool("dump_lcov", {})
    
    assert "lcov" in result
    lcov_content = result["lcov"]
    
    # Basic LCOV format validation
    # LCOV files should contain lines like:
    # TN:<test name>
    # SF:<source file>
    # DA:<line>,<count>
    # end_of_record
    
    if lcov_content:  # If not empty
        # Check for basic LCOV markers
        assert "SF:" in lcov_content or "TN:" in lcov_content or len(lcov_content) == 0, \
            "LCOV format should contain SF: or TN: markers or be empty"


def test_dump_lcov_response_structure(mcp_client):
    """Test that dump_lcov returns expected structure."""
    result = mcp_client.call_tool("dump_lcov", {})
    
    assert "lcov" in result
    assert "contracts" in result or isinstance(result["lcov"], str)


def test_show_coverage_response_time(mcp_client):
    """Test that show_coverage responds within 100ms."""
    times = []
    
    for _ in range(10):
        start = time.perf_counter()
        mcp_client.call_tool("show_coverage", {})
        elapsed = (time.perf_counter() - start) * 1000
        times.append(elapsed)
    
    mean_time = sum(times) / len(times)
    
    print(f"Mean response time: {mean_time:.2f}ms")
    
    assert mean_time < 100, f"Mean response time {mean_time:.2f}ms exceeds 100ms"

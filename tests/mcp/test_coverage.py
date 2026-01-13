"""
Coverage Tool Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T059

Tests for show_coverage and dump_lcov MCP tools using upstream implementation.
"""

import time


def test_show_coverage_returns_data(mcp_client):
    """Test that show_coverage returns coverage report."""
    result = mcp_client.call_tool("show_coverage", {})
    
    # Upstream returns coverage as text in MCP format
    assert "content" in result
    assert len(result["content"]) > 0
    assert "text" in result["content"][0]
    
    coverage_text = result["content"][0]["text"]
    assert isinstance(coverage_text, str)
    assert len(coverage_text) > 0


def test_dump_lcov_format(mcp_client):
    """Test that dump_lcov returns LCOV format data."""
    result = mcp_client.call_tool("dump_lcov", {})
    
    # Upstream returns LCOV data as text in MCP format
    assert "content" in result
    assert len(result["content"]) > 0
    assert "text" in result["content"][0]
    
    lcov_data = result["content"][0]["text"]
    assert isinstance(lcov_data, str)
    
    # LCOV format may be empty or contain SF:/DA: markers
    # Empty is OK if no coverage yet
    if lcov_data.strip():
        # Basic LCOV format validation
        # LCOV files should contain lines like:
        # TN:<test name>
        # SF:<source file>
        # DA:<line>,<count>
        # end_of_record
        assert "SF:" in lcov_data or "TN:" in lcov_data or "end_of_record" in lcov_data, \
            "LCOV format should contain standard markers"


def test_show_coverage_response_time(mcp_client):
    """Test that show_coverage responds within reasonable time."""
    times = []
    
    for _ in range(5):
        start = time.perf_counter()
        mcp_client.call_tool("show_coverage", {})
        elapsed = (time.perf_counter() - start) * 1000
        times.append(elapsed)
    
    mean_time = sum(times) / len(times)
    max_time = max(times)
    
    print(f"Mean response time: {mean_time:.2f}ms, Max: {max_time:.2f}ms")
    
    # Generous timeout for CI environments
    assert mean_time < 500, f"Mean response time {mean_time:.2f}ms exceeds 500ms"


def test_dump_lcov_response_structure(mcp_client):
    """Test that dump_lcov returns valid MCP response structure."""
    result = mcp_client.call_tool("dump_lcov", {})
    
    # MCP format validation
    assert "content" in result
    assert isinstance(result["content"], list)
    assert len(result["content"]) > 0
    assert "text" in result["content"][0]
    assert "type" in result["content"][0]
    assert result["content"][0]["type"] == "text"

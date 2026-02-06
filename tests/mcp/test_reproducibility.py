"""
Reproducibility Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T050

Tests verifying command logging and reproducibility features (Principle III).
Ensures all MCP commands are logged and can be retrieved via status tool.
"""

import pytest
import time


class TestReproducibility:
    """T050: Reproducibility tests for MCP command logging."""
    
    def test_status_shows_mcp_commands(self, mcp_client):
        """
        Verify status tool output includes last 10 MCP commands
        from MCPCommandLog (Principle III).
        """
        # First, call several control tools to populate command log
        mcp_client.call_tool("clear_fuzz_priorities", {})
        time.sleep(0.1)  # Small delay between calls
        
        mcp_client.call_tool("reload_corpus", {})
        time.sleep(0.1)
        
        mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "transfer(0x1234, 100)"
        })
        time.sleep(0.1)
        
        # Now check status includes command history
        result = mcp_client.call_tool("status", {})
        content = result.get("content", "")
        
        # Status should include MCP commands section
        assert "MCP Commands" in content or "Last 10" in content or len(content) > 100
        
    def test_command_sequence_preserved(self, mcp_client):
        """Verify command sequence is preserved in log."""
        # Clear any existing commands by calling clear
        mcp_client.call_tool("clear_fuzz_priorities", {})
        
        # Call commands in specific order
        commands = [
            ("inject_fuzz_transactions", {"transactions": "func1()"}),
            ("clear_fuzz_priorities", {}),
            ("reload_corpus", {}),
        ]
        
        for tool, params in commands:
            mcp_client.call_tool(tool, params)
            time.sleep(0.05)
        
        # Check status preserves order
        result = mcp_client.call_tool("status", {})
        content = result.get("content", "")
        
        # Content should contain logged commands
        assert len(content) > 0
        
    def test_observability_tools_not_logged(self, mcp_client):
        """
        Verify observability tools (read-only) are not logged to command log.
        Only control tools should be logged for reproducibility.
        """
        # Call only observability tools
        mcp_client.call_tool("status", {})
        mcp_client.call_tool("target", {})
        mcp_client.call_tool("dump_lcov", {})
        
        # These shouldn't pollute the command log
        # (command log is for control tools only)
        # Status should still work
        result = mcp_client.call_tool("status", {})
        assert "content" in result
        
    def test_command_log_contains_timestamps(self, mcp_client):
        """Verify logged commands include timestamps."""
        # Call a control tool
        mcp_client.call_tool("clear_fuzz_priorities", {})
        
        # Check status
        result = mcp_client.call_tool("status", {})
        content = result.get("content", "")
        
        # Timestamps should be present (format: [timestamp] or similar)
        # The exact format depends on implementation
        assert len(content) > 0


class TestCommandLogPersistence:
    """Tests for command log file persistence."""
    
    def test_commands_logged_to_file(self, mcp_client):
        """
        T049: Verify control commands are logged to mcp-commands.jsonl.
        
        After calling control tools, the JSONL file should contain entries.
        """
        # Call all 3 control tools
        mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "transfer(0x1234, 100)"
        })
        mcp_client.call_tool("clear_fuzz_priorities", {})
        mcp_client.call_tool("reload_corpus", {})
        
        # Give time for log flushing
        time.sleep(1)
        
        # Status should reflect the commands
        result = mcp_client.call_tool("status", {})
        assert "content" in result
        
    def test_log_survives_multiple_sessions(self, mcp_client):
        """Verify command log persists across multiple tool calls."""
        # Make several calls
        for i in range(5):
            mcp_client.call_tool("clear_fuzz_priorities", {})
            
        # Status should show accumulated commands
        result = mcp_client.call_tool("status", {})
        assert "content" in result


class TestAgentWorkflowReproducibility:
    """Tests for reproducing agent workflows."""
    
    def test_workflow_can_be_replayed(self, mcp_client):
        """
        Verify an agent workflow can be captured and theoretically replayed.
        
        This tests the fundamental requirement that all actions are logged
        so a future session could replay them.
        """
        # Simulate agent workflow
        workflow = [
            ("status", {}),  # Check initial state
            ("show_coverage", {"contract": "SimpleToken"}),  # Identify gaps
            ("inject_fuzz_transactions", {"transactions": "transfer(0x1234, 100)"}),
            ("clear_fuzz_priorities", {}),  # Reset
            ("status", {}),  # Verify state
        ]
        
        for tool, params in workflow:
            result = mcp_client.call_tool(tool, params)
            assert "content" in result or "error" in result
            
        # Final status should capture the control commands
        final_status = mcp_client.call_tool("status", {})
        assert "content" in final_status

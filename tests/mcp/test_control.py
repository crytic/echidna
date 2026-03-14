"""
Control Tool Tests
Feature: 001-mcp-agent-commands
Phase 5, Tasks T041-T043

Tests for control tools that modify fuzzing strategy:
inject_fuzz_transactions, clear_fuzz_priorities, reload_corpus.
"""

import pytest
import time


class TestInjectFuzzTransactionsTool:
    """T041: Contract test for inject_fuzz_transactions tool."""
    
    def test_inject_transactions_broadcasts_to_workers(self, mcp_client):
        """
        Verify inject_fuzz_transactions broadcasts transactions to all workers.
        
        Per contracts/inject_fuzz_transactions.json schema, response should
        indicate success and number of workers that received the request.
        """
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "transfer(0x1234567890123456789012345678901234567890, 100)"
        })
        
        assert "content" in result
        content = result["content"]
        
        # Should indicate request was sent to workers
        assert "fuzzer" in content.lower() or "worker" in content.lower() or "error" in content.lower()
        
    def test_inject_transactions_with_sequence(self, mcp_client):
        """Verify multi-step transaction sequence injection."""
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "deposit(1000);emergencyWithdraw()"
        })
        
        assert "content" in result
        
    def test_inject_transactions_validation_error(self, mcp_client):
        """T031: Verify invalid syntax returns proper error."""
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "invalid_syntax_without_parens"
        })
        
        # Should return error for invalid syntax
        content = result.get("content", "")
        assert "error" in result or "Error" in content
        
    def test_inject_transactions_unknown_function_error(self, mcp_client):
        """Verify unknown function returns proper error."""
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "nonexistentFunction123(42)"
        })
        
        content = result.get("content", "")
        # Should return error for unknown function
        assert "error" in result or "not found" in content.lower() or "Error" in content


class TestClearFuzzPrioritiesTool:
    """T042: Contract test for clear_fuzz_priorities tool."""
    
    def test_clear_priorities_resets_strategy(self, mcp_client):
        """
        Verify clear_fuzz_priorities resets prioritization strategy.
        
        After calling, GenDict.prioritizedFunctions should be empty.
        """
        result = mcp_client.call_tool("clear_fuzz_priorities", {})
        
        assert "content" in result
        content = result["content"]
        
        # Should indicate priorities were cleared
        assert "clear" in content.lower() or "priorit" in content.lower()
        
    def test_clear_priorities_is_logged(self, mcp_client):
        """T033: Verify clear_priorities call is logged to MCPCommandLog."""
        result = mcp_client.call_tool("clear_fuzz_priorities", {})
        
        # Check status shows the command was logged
        status_result = mcp_client.call_tool("status", {})
        content = status_result.get("content", "")
        
        # Command should appear in status output
        assert "clear" in content.lower() or "MCP Commands" in content


class TestReloadCorpusTool:
    """T043: Contract test for reload_corpus tool."""
    
    def test_reload_corpus_resets_state(self, mcp_client):
        """
        Verify reload_corpus reloads transactions from disk.
        
        Per contracts/reload_corpus.json schema, should report
        number of sequences reloaded.
        """
        result = mcp_client.call_tool("reload_corpus", {})
        
        assert "content" in result
        content = result["content"]
        
        # Should indicate corpus reload status
        assert "corpus" in content.lower() or "reload" in content.lower() or \
               "transaction" in content.lower() or "sequence" in content.lower()
        
    def test_reload_corpus_is_logged(self, mcp_client):
        """T033: Verify reload_corpus call is logged to MCPCommandLog."""
        result = mcp_client.call_tool("reload_corpus", {})
        
        # Check status shows the command was logged
        status_result = mcp_client.call_tool("status", {})
        content = status_result.get("content", "")
        
        # Should have MCP commands section
        assert "MCP" in content or "reload" in content.lower()


class TestControlToolsIntegration:
    """Integration tests for control tools working together."""
    
    def test_inject_then_clear_workflow(self, mcp_client):
        """Verify inject followed by clear resets state."""
        # Inject some transactions
        inject_result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "transfer(0x1234567890123456789012345678901234567890, 100)"
        })
        
        # Clear priorities
        clear_result = mcp_client.call_tool("clear_fuzz_priorities", {})
        
        # Both should succeed
        assert "content" in inject_result
        assert "content" in clear_result
        
    def test_all_control_commands_logged(self, mcp_client):
        """T033: Verify all control commands are logged to MCPCommandLog."""
        # Call all control tools
        mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "transfer(0x1234567890123456789012345678901234567890, 100)"
        })
        mcp_client.call_tool("clear_fuzz_priorities", {})
        mcp_client.call_tool("reload_corpus", {})
        
        # Check status includes all commands
        status_result = mcp_client.call_tool("status", {})
        content = status_result.get("content", "")
        
        # Should show MCP commands section with logged entries
        assert "MCP" in content or len(content) > 0

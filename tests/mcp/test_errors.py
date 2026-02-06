"""
Error Handling Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T045

Tests verifying JSON-RPC 2.0 error responses for invalid inputs.
Ensures fuzzer never crashes on bad input (FR-013).
"""

import pytest


class TestInvalidInputErrors:
    """T045: Error handling tests for invalid inputs."""
    
    def test_invalid_transaction_syntax_returns_error(self, mcp_client):
        """
        Inject "transfer(0xZZZ, -100)" and verify JSON-RPC 2.0 error response
        with code/message (Clarification #7).
        """
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "transfer(0xZZZ, -100)"
        })
        
        # Should return error for invalid hex and negative value
        content = result.get("content", "")
        assert "error" in result or "Error" in content or "Failed" in content
        
    def test_empty_transaction_sequence_error(self, mcp_client):
        """Verify empty transaction sequence returns error."""
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": ""
        })
        
        content = result.get("content", "")
        # Empty string should fail parsing
        assert "error" in result or "Error" in content
        
    def test_malformed_function_call_error(self, mcp_client):
        """Verify malformed function call returns error."""
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "noParens"
        })
        
        content = result.get("content", "")
        assert "error" in result or "Error" in content or "parse" in content.lower()
        
    def test_unclosed_parentheses_error(self, mcp_client):
        """Verify unclosed parentheses returns error."""
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "transfer(100"
        })
        
        content = result.get("content", "")
        assert "error" in result or "Error" in content
        
    def test_invalid_address_format_error(self, mcp_client):
        """Verify invalid address format returns error."""
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "transfer(not_an_address, 100)"
        })
        
        content = result.get("content", "")
        # Should return error for invalid address
        assert "error" in result or "Error" in content


class TestUnknownToolErrors:
    """Error handling for unknown tools."""
    
    def test_unknown_tool_returns_error(self, mcp_client):
        """Verify calling unknown tool returns proper error."""
        try:
            result = mcp_client.call_tool("nonexistent_tool", {})
            # If we get here, check for error in response
            assert "error" in result or "Error" in result.get("content", "")
        except RuntimeError as e:
            # Expected: tool not found error
            assert "error" in str(e).lower() or "unknown" in str(e).lower()


class TestMissingParameterErrors:
    """Error handling for missing required parameters."""
    
    def test_show_coverage_missing_contract_error(self, mcp_client):
        """Verify show_coverage without contract param returns error."""
        result = mcp_client.call_tool("show_coverage", {})
        
        content = result.get("content", "")
        # Should return error for missing contract
        assert "error" in result or "Error" in content
        
    def test_inject_transactions_missing_transactions_error(self, mcp_client):
        """Verify inject_fuzz_transactions without transactions param returns error."""
        result = mcp_client.call_tool("inject_fuzz_transactions", {})
        
        content = result.get("content", "")
        # Should return error for missing transactions
        assert "error" in result or "Error" in content


class TestEdgeCaseErrors:
    """Edge case error handling."""
    
    def test_very_long_transaction_sequence_error(self, mcp_client):
        """Verify extremely long transaction sequence is handled gracefully."""
        # Create a very long transaction sequence
        long_tx = ";".join(["transfer(0x1234, 1)" for _ in range(1000)])
        
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": long_tx
        })
        
        # Should either succeed or return graceful error, not crash
        assert "content" in result or "error" in result
        
    def test_special_characters_in_input(self, mcp_client):
        """Verify special characters don't cause crashes."""
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "transfer(\\x00\\x01, 100)"
        })
        
        # Should return error, not crash
        assert "content" in result or "error" in result
        
    def test_unicode_in_input(self, mcp_client):
        """Verify unicode characters don't cause crashes."""
        result = mcp_client.call_tool("inject_fuzz_transactions", {
            "transactions": "transfer(🎉, 💰)"
        })
        
        # Should return error, not crash
        assert "content" in result or "error" in result

"""
MCP Client Wrapper - Enhanced Client for Testing
Feature: 001-mcp-agent-commands
Phase 5, Task T056

Provides MCPClientV2 with convenience methods for each MCP tool.
"""

import httpx
from typing import Dict, Any, List, Optional


class MCPClientV2:
    """
    Enhanced MCP client with convenience methods for each tool.
    
    Wraps httpx and provides typed methods for all 7 active MCP tools.
    Note: read_logs tool exists but is currently commented out in upstream.
    """
    
    def __init__(self, base_url: str = "http://localhost:8080"):
        self.base_url = base_url
        self.client = httpx.Client(timeout=30.0)
        
    def _call_tool(self, tool_name: str, parameters: Dict[str, Any]) -> Dict[str, Any]:
        """Internal method to call MCP tool via JSON-RPC."""
        payload = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "tools/call",
            "params": {
                "name": tool_name,
                "arguments": parameters
            }
        }
        
        response = self.client.post(f"{self.base_url}/mcp", json=payload)
        response.raise_for_status()
        
        result = response.json()
        
        if "error" in result:
            raise RuntimeError(f"MCP tool error: {result['error']}")
            
        return result.get("result", {})
    
    # Observability Tools
    
    def call_status(self) -> Dict[str, Any]:
        """
        Get fuzzing campaign status and metrics.
        
        Returns:
            Campaign status including corpus size, iterations, coverage, etc.
        """
        return self._call_tool("status", {})
    
    def call_show_coverage(self, contract: Optional[str] = None) -> Dict[str, Any]:
        """
        Get code coverage report for a specific contract.
        
        Args:
            contract: Contract name to show coverage for (optional)
            
        Returns:
            Coverage report with annotated source code
        """
        params = {}
        if contract:
            params["contract"] = contract
        return self._call_tool("show_coverage", params)
    
    def call_target(self) -> Dict[str, Any]:
        """
        Get target contract name and ABI.
        
        Returns:
            Target contract information with ABI
        """
        return self._call_tool("target", {})
    
    def call_dump_lcov(self) -> Dict[str, Any]:
        """
        Dump coverage in LCOV format.
        
        Returns:
            LCOV formatted coverage data
        """
        return self._call_tool("dump_lcov", {})
    
    def call_reload_corpus(self) -> Dict[str, Any]:
        """
        Reload corpus from disk without replaying transactions.
        
        Returns:
            Status of corpus reload operation
        """
        return self._call_tool("reload_corpus", {})
    
    # Control Tools
    
    def call_inject_fuzz_transactions(self, transactions: str) -> Dict[str, Any]:
        """
        Inject transaction sequence to prioritize during fuzzing.
        
        Args:
            transactions: Newline-separated transaction sequence
                         e.g., "transfer(0x123..., 100)\\nmint(1000, 0x456...)"
            
        Returns:
            Status of injection operation
        """
        return self._call_tool("inject_fuzz_transactions", {
            "transactions": transactions
        })
    
    def call_clear_fuzz_priorities(self) -> Dict[str, Any]:
        """
        Clear all function prioritization.
        
        Returns:
            Status of clear operation
        """
        return self._call_tool("clear_fuzz_priorities", {})
    
    def list_tools(self) -> Dict[str, Any]:
        """
        List all available MCP tools.
        
        Returns:
            {"tools": [...]}
        """
        payload = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "tools/list",
            "params": {}
        }
        
        response = self.client.post(f"{self.base_url}/mcp", json=payload)
        response.raise_for_status()
        
        return response.json().get("result", {})
    
    def close(self):
        """Close HTTP client."""
        self.client.close()

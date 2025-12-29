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
    
    Wraps httpx and provides typed methods for all 9 MCP tools.
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
    
    def call_read_logs(self, max_count: int = 100, since_timestamp: Optional[int] = None) -> Dict[str, Any]:
        """
        Read event logs from Echidna campaign.
        
        Args:
            max_count: Maximum number of events to return
            since_timestamp: Optional Unix timestamp to filter events
            
        Returns:
            {"events": [...], "count": int, "timestamp": int}
        """
        params = {"max_count": max_count}
        if since_timestamp:
            params["since_timestamp"] = since_timestamp
        return self._call_tool("read_logs", params)
    
    def call_show_coverage(self) -> Dict[str, Any]:
        """
        Get current code coverage statistics.
        
        Returns:
            {"coverage_points": int, "contracts": [...], "timestamp": int}
        """
        return self._call_tool("show_coverage", {})
    
    def call_dump_lcov(self) -> Dict[str, Any]:
        """
        Dump coverage in LCOV format.
        
        Returns:
            {"lcov": str, "contracts": int}
        """
        return self._call_tool("dump_lcov", {})
    
    def call_get_corpus_size(self) -> Dict[str, Any]:
        """
        Get current corpus size.
        
        Returns:
            {"corpus_size": int}
        """
        return self._call_tool("get_corpus_size", {})
    
    def call_inspect_corpus(self, offset: int = 0, limit: int = 10) -> Dict[str, Any]:
        """
        Inspect transactions in corpus with pagination.
        
        Args:
            offset: Starting index
            limit: Maximum number of transactions to return
            
        Returns:
            {"transactions": [...], "total": int, "offset": int, "limit": int}
        """
        return self._call_tool("inspect_corpus_transactions", {
            "offset": offset,
            "limit": limit
        })
    
    def call_find_transaction(self, search_query: str) -> Dict[str, Any]:
        """
        Search for transactions in corpus.
        
        Args:
            search_query: Function signature or substring to search for
            
        Returns:
            {"matches": [...], "count": int}
        """
        return self._call_tool("find_transaction_in_corpus", {
            "search_query": search_query
        })
    
    # Control Tools
    
    def call_inject_transaction(self, transactions: List[str]) -> Dict[str, Any]:
        """
        Inject custom transactions into fuzzing campaign.
        
        Args:
            transactions: List of Solidity-like function call strings
                         e.g., ["transfer(0x123..., 100)", "approve(0x456..., 50)"]
            
        Returns:
            {"injected": bool, "transaction_count": int, "worker_id": int}
        """
        return self._call_tool("inject_transaction", {
            "transactions": transactions
        })
    
    def call_prioritize_function(self, function_signature: str) -> Dict[str, Any]:
        """
        Set priority for a specific function signature.
        
        Args:
            function_signature: Function signature to prioritize (e.g., "balanceOf(address)")
            
        Returns:
            {"prioritized": bool, "function_signature": str, "worker_ids": [int]}
        """
        return self._call_tool("prioritize_function", {
            "function_signature": function_signature
        })
    
    def call_clear_priorities(self) -> Dict[str, Any]:
        """
        Clear all function prioritization.
        
        Returns:
            {"cleared": bool, "worker_ids": [int]}
        """
        return self._call_tool("clear_priorities", {})
    
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

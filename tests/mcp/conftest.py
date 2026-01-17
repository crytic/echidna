"""
MCP Test Suite Configuration
Feature: 001-mcp-agent-commands
Phase 1, Task T007 | Phase 5, Task T055

Provides pytest fixtures for MCP integration testing.
"""

import pytest
import httpx
import subprocess
import time
import os
import socket
from typing import Dict, Any


class MCPClient:
    """
    Simple MCP client for testing.
    
    Communicates with Echidna MCP server via HTTP + JSON-RPC.
    """
    
    def __init__(self, base_url: str = "http://localhost:8080"):
        self.base_url = base_url
        self.client = httpx.Client(timeout=30.0)
        
    def call_tool(self, tool_name: str, parameters: Dict[str, Any]) -> Dict[str, Any]:
        """
        Call an MCP tool and return the response.
        
        Args:
            tool_name: Name of the tool (e.g., "read_logs", "inject_transaction")
            parameters: Tool parameters as key-value dict
            
        Returns:
            Tool response as JSON dict
            
        Raises:
            httpx.HTTPError: If request fails
        """
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
    
    def list_tools(self) -> Dict[str, Any]:
        """
        List all available MCP tools.
        
        Returns:
            List of tools with names and descriptions
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
        """Close HTTP client"""
        self.client.close()


@pytest.fixture
def mcp_client():
    """
    Pytest fixture providing an MCP client connected to localhost:8080.
    
    Usage:
        def test_read_logs(mcp_client):
            result = mcp_client.call_tool("read_logs", {"max_count": 10})
            assert "events" in result
    """
    client = MCPClient("http://localhost:8080")
    yield client
    client.close()


@pytest.fixture
def mcp_client_custom_port():
    """
    Pytest fixture factory for custom MCP server ports.
    
    Usage:
        def test_custom_port(mcp_client_custom_port):
            client = mcp_client_custom_port(8081)
            result = client.call_tool("read_logs", {})
            assert result is not None
    """
    def _make_client(port: int = 8080):
        return MCPClient(f"http://localhost:{port}")
    
    return _make_client


# Test configuration
TEST_TIMEOUT = 30  # seconds
MCP_DEFAULT_PORT = 8080


@pytest.fixture
def echidna_campaign_running(request, tmp_path):
    """
    Pytest fixture that spawns an Echidna campaign with MCP server.
    
    Args:
        request: pytest fixture request (can provide 'contract_path' marker)
        tmp_path: pytest fixture for temporary directory
    
    Yields:
        dict with keys: 'port' (int), 'process' (subprocess.Popen)
    
    Usage:
        @pytest.mark.parametrize('contract_path', ['tests/mcp/contracts/SimpleToken.sol'])
        def test_with_campaign(echidna_campaign_running):
            port = echidna_campaign_running['port']
            # campaign is running on port...
    """
    # Get contract path from marker or use default
    marker = request.node.get_closest_marker('contract_path')
    contract_path = marker.args[0] if marker else 'tests/mcp/contracts/EchidnaMCPTest.sol'
    
    port = MCP_DEFAULT_PORT
    
    # Build command - use 'echidna' binary (not 'echidna-test')
    cmd = [
        'echidna',
        contract_path,
        '--server', str(port),
        '--test-mode', 'assertion',
        '--test-limit', '1000000000',
        '--format', 'text'  # Required to avoid TUI blocking
    ]

    # Check for contract_name marker
    name_marker = request.node.get_closest_marker('contract_name')
    if name_marker:
        cmd.extend(['--contract', name_marker.args[0]])
    elif 'EchidnaMCPTest.sol' in contract_path:
        cmd.extend(['--contract', 'EchidnaMCPTest'])

    # Check for use_tmp_corpus marker
    if request.node.get_closest_marker('use_tmp_corpus'):
        corpus_dir = tmp_path / "corpus"
        corpus_dir.mkdir(exist_ok=True)
        cmd.extend(['--corpus-dir', str(corpus_dir)])
    
    # Ensure we use the locally built echidna
    env = os.environ.copy()
    home = os.path.expanduser("~")
    local_bin = os.path.join(home, ".local", "bin")
    env["PATH"] = f"{local_bin}:{env.get('PATH', '')}"

    # Start Echidna
    process = subprocess.Popen(
        cmd,
        stdout=subprocess.PIPE,
        stderr=subprocess.PIPE,
        text=True,
        env=env
    )
    
    # Wait for server to be ready (max 10 seconds)
    start_time = time.time()
    server_ready = False

    while time.time() - start_time < 10:
        try:
            # Try to connect to the TCP port
            with socket.create_connection(("localhost", port), timeout=1.0):
                server_ready = True
                break
        except (OSError, ConnectionRefusedError):
            time.sleep(0.5)

    if not server_ready:
        process.terminate()
        stdout, stderr = process.communicate(timeout=5)
        print(f"Echidna stdout:\n{stdout}")
        print(f"Echidna stderr:\n{stderr}")
        raise RuntimeError(f'Echidna MCP server did not start within 10 seconds')
    
    # Yield to test
    yield {
        'port': port,
        'process': process
    }
    
    # Cleanup
    process.terminate()
    try:
        stdout, stderr = process.communicate(timeout=5)
        print(f"Echidna stdout:\n{stdout}")
        print(f"Echidna stderr:\n{stderr}")
    except subprocess.TimeoutExpired:
        process.kill()
        process.wait()

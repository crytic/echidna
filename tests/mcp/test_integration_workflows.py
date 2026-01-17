"""
Integration Workflow Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T063

Multi-step agent workflow tests using upstream's 7 tools.
"""

import pytest
import time
import asyncio
import httpx


def test_agent_workflow_observe_then_inject(mcp_client):
    """
    Test agent workflow: observe → inject → observe again.
    
    Simulates an agent that:
    1. Checks status to understand current state
    2. Injects a targeted transaction
    3. Checks status again to verify progress
    """
    # Step 1: Observe initial state
    status1 = mcp_client.call_tool("status", {})
    status1_text = status1["content"][0]["text"]
    
    print(f"Initial status: {status1_text[:100]}...")
    
    # Step 2: Inject transaction
    inject_result = mcp_client.call_tool("inject_fuzz_transactions", {
        "transactions": "transfer(0x1234567890123456789012345678901234567890, 999)"
    })
    
    response_text = inject_result["content"][0]["text"]
    assert "requested" in response_text.lower() or "fuzzing" in response_text.lower()
    
    # Step 3: Wait and observe again
    time.sleep(2)
    
    status2 = mcp_client.call_tool("status", {})
    status2_text = status2["content"][0]["text"]
    
    print(f"Updated status: {status2_text[:100]}...")
    
    # Status should be returned successfully
    assert status2_text


def test_agent_workflow_coverage_analysis(mcp_client):
    """
    Test agent workflow: check coverage → analyze → inject.
    
    Simulates an agent that:
    1. Gets target contract info
    2. Checks current coverage
    3. Injects transactions to increase coverage
    """
    # Step 1: Get target info
    target = mcp_client.call_tool("target", {})
    target_text = target["content"][0]["text"]
    
    print(f"Target: {target_text[:100]}...")
    
    # Step 2: Get current coverage
    coverage1 = mcp_client.call_tool("show_coverage", {})
    coverage1_text = coverage1["content"][0]["text"]
    
    print(f"Initial coverage (first 200 chars): {coverage1_text[:200]}...")
    
    # Step 3: Inject diverse transactions
    inject_result = mcp_client.call_tool("inject_fuzz_transactions", {
        "transactions": "transfer(0x1111111111111111111111111111111111111111, 100)\\napprove(0x2222222222222222222222222222222222222222, 200)"
    })
    
    response_text = inject_result["content"][0]["text"]
    assert "requested" in response_text.lower()
    
    # Step 4: Wait for execution
    time.sleep(2)
    
    # Step 5: Check coverage again
    coverage2 = mcp_client.call_tool("show_coverage", {})
    coverage2_text = coverage2["content"][0]["text"]
    
    print(f"Updated coverage (first 200 chars): {coverage2_text[:200]}...")
    
    # Coverage text should be returned
    assert coverage2_text


@pytest.mark.asyncio
async def test_concurrent_tool_calls():
    """
    Test calling multiple tools concurrently.
    
    Verifies no race conditions when multiple agents call tools simultaneously.
    """
    async with httpx.AsyncClient(timeout=30.0) as client:
        base_url = "http://localhost:8080"
        
        async def call_tool(tool_name: str, params: dict):
            """Helper to call MCP tool asynchronously."""
            payload = {
                "jsonrpc": "2.0",
                "id": 1,
                "method": "tools/call",
                "params": {
                    "name": tool_name,
                    "arguments": params
                }
            }
            response = await client.post(f"{base_url}/mcp", json=payload)
            return response.json()
        
        # Call 3 observability tools concurrently
        tasks = [
            call_tool("status", {}),
            call_tool("show_coverage", {}),
            call_tool("target", {})
        ]
        
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # All should succeed
        for result in results:
            assert not isinstance(result, Exception), f"Tool call failed: {result}"
            assert "result" in result or "error" not in result


def test_agent_workflow_full_cycle(mcp_client):
    """
    Test complete agent workflow combining all capabilities.
    
    Simulates a sophisticated agent that:
    1. Observes initial state (status + coverage)
    2. Gets target contract info
    3. Injects targeted transactions
    4. Monitors results
    5. Clears priorities and exports coverage
    """
    # Phase 1: Initial observation
    status1 = mcp_client.call_tool("status", {})
    coverage1 = mcp_client.call_tool("show_coverage", {})
    target = mcp_client.call_tool("target", {})
    
    status_text = status1["content"][0]["text"]
    print(f"Initial status: {status_text[:100]}...")
    
    # Phase 2: Inject targeted transactions
    inject_result = mcp_client.call_tool("inject_fuzz_transactions", {
        "transactions": "transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, 42)"
    })
    
    response_text = inject_result["content"][0]["text"]
    assert "requested" in response_text.lower()
    
    # Phase 3: Wait and monitor
    time.sleep(2)
    
    status2 = mcp_client.call_tool("status", {})
    status2_text = status2["content"][0]["text"]
    
    print(f"After injection: {status2_text[:100]}...")
    
    # Phase 4: Clear priorities and export coverage
    clear_result = mcp_client.call_tool("clear_fuzz_priorities", {})
    clear_text = clear_result["content"][0]["text"]
    assert "requested" in clear_text.lower() or "clearing" in clear_text.lower()
    
    # Export coverage in LCOV format
    lcov_result = mcp_client.call_tool("dump_lcov", {})
    lcov_data = lcov_result["content"][0]["text"]
    assert isinstance(lcov_data, str)  # LCOV data is a string
    
    # Workflow completed successfully
    print("✅ Full cycle test completed")

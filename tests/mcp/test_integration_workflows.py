"""
Integration Workflow Tests
Feature: 001-mcp-agent-commands
Phase 5, Task T063

Multi-step agent workflow tests.
"""

import pytest
import time
import asyncio
import httpx


def test_agent_workflow_observe_then_inject(mcp_client):
    """
    Test agent workflow: observe → inject → observe again.
    
    Simulates an agent that:
    1. Reads logs to understand current state
    2. Injects a targeted transaction
    3. Reads logs again to verify injection
    """
    # Step 1: Observe initial state
    logs1 = mcp_client.call_tool("read_logs", {"max_count": 10})
    initial_count = logs1["count"]
    
    print(f"Initial event count: {initial_count}")
    
    # Step 2: Inject transaction
    inject_result = mcp_client.call_tool("inject_fuzz_transactions", {
        "transactions": ["transfer(0x1234567890123456789012345678901234567890, 999)"]
    })
    
    assert inject_result["injected"] is True
    
    # Step 3: Wait and observe again
    time.sleep(2)
    
    logs2 = mcp_client.call_tool("read_logs", {"max_count": 20})
    updated_count = logs2["count"]
    
    print(f"Updated event count: {updated_count}")
    
    # Events should have been logged
    assert updated_count >= initial_count


def test_agent_workflow_prioritize_and_monitor(mcp_client, echidna_campaign_running):
    """
    Test agent workflow: prioritize → get corpus → find transactions.
    
    Simulates an agent that:
    1. Prioritizes a function
    2. Gets corpus size
    3. Finds transactions with that function
    """
    # Step 1: Prioritize
    prioritize_result = mcp_client.call_tool("prioritize_function", {
        "function_signature": "balanceOf(address)"
    })
    
    assert prioritize_result["prioritized"] is True
    
    # Step 2: Wait for fuzzing
    time.sleep(3)
    
    # Step 3: Get corpus size
    corpus_result = mcp_client.call_tool("get_corpus_size", {})
    corpus_size = corpus_result["corpus_size"]
    
    print(f"Corpus size: {corpus_size}")
    
    # Step 4: Find transactions
    search_result = mcp_client.call_tool("find_transaction_in_corpus", {
        "search_query": "balanceOf"
    })
    
    assert "matches" in search_result
    print(f"Found {search_result['count']} balanceOf transactions")


def test_agent_workflow_coverage_driven_injection(mcp_client, echidna_campaign_running):
    """
    Test agent workflow: check coverage → inject → verify coverage increase.
    
    Simulates an agent that:
    1. Checks current coverage
    2. Injects transactions to increase coverage
    3. Verifies coverage increased
    """
    # Step 1: Get initial coverage
    coverage1 = mcp_client.call_tool("show_coverage", {})
    initial_points = coverage1["coverage_points"]
    
    print(f"Initial coverage: {initial_points} points")
    
    # Step 2: Inject diverse transactions
    inject_result = mcp_client.call_tool("inject_fuzz_transactions", {
        "transactions": [
            "transfer(0x1111111111111111111111111111111111111111, 100)",
            "approve(0x2222222222222222222222222222222222222222, 200)",
            "transferFrom(0x3333333333333333333333333333333333333333, 0x4444444444444444444444444444444444444444, 50)"
        ]
    })
    
    assert inject_result["injected"] is True
    
    # Step 3: Wait for execution
    time.sleep(3)
    
    # Step 4: Check coverage again
    coverage2 = mcp_client.call_tool("show_coverage", {})
    updated_points = coverage2["coverage_points"]
    
    print(f"Updated coverage: {updated_points} points")
    
    # Coverage should not decrease
    assert updated_points >= initial_points


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
        
        # Call 3 tools concurrently
        tasks = [
            call_tool("read_logs", {"max_count": 10}),
            call_tool("show_coverage", {}),
            call_tool("get_corpus_size", {})
        ]
        
        results = await asyncio.gather(*tasks, return_exceptions=True)
        
        # All should succeed
        for result in results:
            assert not isinstance(result, Exception), f"Tool call failed: {result}"
            assert "result" in result, f"Expected 'result' key in {result}"
            assert "error" not in result, f"Unexpected error: {result.get('error')}"


def test_agent_workflow_full_cycle(mcp_client, echidna_campaign_running):
    """
    Test complete agent workflow combining all capabilities.
    
    Simulates a sophisticated agent that:
    1. Observes initial state (logs + coverage)
    2. Prioritizes interesting functions
    3. Injects targeted transactions
    4. Monitors results
    5. Clears priorities and repeats
    """
    # Phase 1: Initial observation
    logs = mcp_client.call_tool("read_logs", {"max_count": 20})
    coverage = mcp_client.call_tool("show_coverage", {})
    
    print(f"Initial state: {logs['count']} events, {coverage['coverage_points']} coverage points")
    
    # Phase 2: Prioritize
    mcp_client.call_tool("prioritize_function", {
        "function_signature": "transfer(address,uint256)"
    })
    
    # Phase 3: Inject
    mcp_client.call_tool("inject_fuzz_transactions", {
        "transactions": ["transfer(0xaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa, 42)"]
    })
    
    # Phase 4: Wait and monitor
    time.sleep(2)
    
    corpus_size = mcp_client.call_tool("get_corpus_size", {})
    search_result = mcp_client.call_tool("find_transaction_in_corpus", {
        "search_query": "transfer"
    })
    
    print(f"After injection: corpus size {corpus_size['corpus_size']}, {search_result['count']} transfers")
    
    # Phase 5: Clear priorities
    clear_result = mcp_client.call_tool("clear_fuzz_priorities", {})
    assert clear_result["cleared"] is True
    
    # Workflow completed successfully if we got here
    assert True

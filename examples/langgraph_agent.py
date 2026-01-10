#!/usr/bin/env python3
"""
Advanced LangGraph Agent for Echidna MCP Integration

This demonstrates a sophisticated AI agent using LangGraph that can:
- Analyze coverage reports
- Generate intelligent transaction sequences
- Make autonomous decisions about fuzzing strategy
- Learn from fuzzing results

Requirements:
    pip install langchain langgraph langchain-anthropic httpx
"""

import httpx
import json
from typing import TypedDict, Annotated, List
from langchain_anthropic import ChatAnthropic
from langgraph.graph import StateGraph, END
from langgraph.prebuilt import ToolNode


# State definition
class AgentState(TypedDict):
    """State for the fuzzing agent."""
    messages: List[dict]
    current_coverage: int
    iterations: int
    corpus_size: int
    stagnation_count: int
    last_action: str
    fuzzing_complete: bool


class EchidnaLangGraphAgent:
    """Advanced LangGraph agent for Echidna MCP integration."""
    
    def __init__(self, mcp_url: str = "http://localhost:8080/mcp", model: str = "claude-3-5-sonnet-20241022"):
        self.mcp_url = mcp_url
        self.client = httpx.Client(timeout=30.0)
        self.llm = ChatAnthropic(model=model, temperature=0.7)
        
        # Build the agent graph
        self.graph = self._build_graph()
    
    def _call_mcp_tool(self, tool_name: str, arguments: dict = None) -> dict:
        """Call MCP tool via JSON-RPC."""
        payload = {
            "jsonrpc": "2.0",
            "id": 1,
            "method": "tools/call",
            "params": {
                "name": tool_name,
                "arguments": arguments or {}
            }
        }
        
        try:
            response = self.client.post(self.mcp_url, json=payload)
            result = response.json()
            return result.get("result", {})
        except Exception as e:
            return {"error": str(e)}
    
    def _analyze_state(self, state: AgentState) -> AgentState:
        """Analyze current fuzzing state and decide next action."""
        print("\n🔍 Analyzing fuzzing state...")
        
        # Get current status
        status_result = self._call_mcp_tool("status")
        status_text = status_result.get("content", [{}])[0].get("text", "")
        
        # Parse metrics
        lines = status_text.split("\n")
        for line in lines:
            if "Coverage:" in line:
                state["current_coverage"] = int(line.split(": ")[1])
            elif "Iterations:" in line:
                iters = line.split(": ")[1].split("/")[0]
                state["iterations"] = int(iters)
            elif "Corpus Size:" in line:
                state["corpus_size"] = int(line.split(": ")[1])
        
        print(f"   Coverage: {state['current_coverage']} instructions")
        print(f"   Iterations: {state['iterations']}")
        print(f"   Corpus: {state['corpus_size']} sequences")
        
        return state
    
    def _decide_action(self, state: AgentState) -> str:
        """Decide what action to take based on state."""
        print("\n🤖 AI deciding next action...")
        
        # Check if coverage is stagnating
        if state["stagnation_count"] >= 3:
            print("   Decision: Coverage stagnant → Inject targeted transactions")
            return "inject"
        elif state["iterations"] % 50000 == 0:
            print("   Decision: Periodic reset → Clear priorities")
            return "reset"
        else:
            print("   Decision: Continue monitoring")
            return "monitor"
    
    def _inject_transactions(self, state: AgentState) -> AgentState:
        """Generate and inject intelligent transaction sequences."""
        print("\n💉 Generating intelligent transactions...")
        
        # Get coverage report for context
        coverage_result = self._call_mcp_tool("show_coverage", {"contract": "EchidnaMCPTest"})
        coverage_text = coverage_result.get("content", [{}])[0].get("text", "")
        
        # Use LLM to generate targeted transactions
        prompt = f"""You are a smart contract security expert analyzing fuzzing coverage.

Current Coverage Report:
{coverage_text[:1000]}...

Current Metrics:
- Coverage: {state['current_coverage']} instructions
- Iterations: {state['iterations']}
- Corpus: {state['corpus_size']} sequences

Generate 3 targeted transaction sequences that could increase coverage.
Focus on:
1. Edge cases (zero values, max values, boundary conditions)
2. Uncovered code paths
3. Complex state transitions

Format each transaction on a new line, like:
transfer(0x1234567890123456789012345678901234567890, 100)

Transaction sequences:"""
        
        # Get LLM suggestions
        response = self.llm.invoke(prompt)
        suggested_txs = response.content.strip().split("\n")
        
        # Inject each transaction
        for tx in suggested_txs[:3]:  # Limit to 3
            tx = tx.strip()
            if tx and not tx.startswith("#") and "(" in tx:
                print(f"   Injecting: {tx}")
                self._call_mcp_tool("inject_fuzz_transactions", {"transactions": tx})
        
        state["last_action"] = "inject"
        state["stagnation_count"] = 0
        return state
    
    def _reset_priorities(self, state: AgentState) -> AgentState:
        """Clear function priorities to encourage exploration."""
        print("\n🔄 Clearing function priorities...")
        self._call_mcp_tool("clear_fuzz_priorities")
        state["last_action"] = "reset"
        return state
    
    def _build_graph(self) -> StateGraph:
        """Build the agent decision graph."""
        workflow = StateGraph(AgentState)
        
        # Add nodes
        workflow.add_node("analyze", self._analyze_state)
        workflow.add_node("decide", lambda s: {**s, "action": self._decide_action(s)})
        workflow.add_node("inject", self._inject_transactions)
        workflow.add_node("reset", self._reset_priorities)
        
        # Add edges
        workflow.set_entry_point("analyze")
        workflow.add_edge("analyze", "decide")
        
        # Conditional edges based on decision
        def route_action(state):
            action = state.get("action", "monitor")
            if action == "inject":
                return "inject"
            elif action == "reset":
                return "reset"
            else:
                return END
        
        workflow.add_conditional_edges("decide", route_action, {
            "inject": "inject",
            "reset": "reset",
            END: END
        })
        
        workflow.add_edge("inject", END)
        workflow.add_edge("reset", END)
        
        return workflow.compile()
    
    def run_autonomous(self, duration_minutes: int = 10, check_interval: int = 60):
        """Run autonomous fuzzing guidance."""
        import time
        
        print(f"🚀 Starting LangGraph autonomous agent")
        print(f"   Duration: {duration_minutes} minutes")
        print(f"   Check interval: {check_interval} seconds")
        print("=" * 60)
        
        start_time = time.time()
        end_time = start_time + (duration_minutes * 60)
        
        state = AgentState(
            messages=[],
            current_coverage=0,
            iterations=0,
            corpus_size=0,
            stagnation_count=0,
            last_action="",
            fuzzing_complete=False
        )
        
        iteration = 0
        last_coverage = 0
        
        while time.time() < end_time:
            iteration += 1
            elapsed = int(time.time() - start_time)
            
            print(f"\n{'='*60}")
            print(f"📊 Iteration {iteration} (elapsed: {elapsed}s)")
            print(f"{'='*60}")
            
            # Run the agent graph
            result = self.graph.invoke(state)
            
            # Update state
            if result["current_coverage"] == last_coverage:
                result["stagnation_count"] += 1
            else:
                result["stagnation_count"] = 0
            
            last_coverage = result["current_coverage"]
            state = result
            
            # Wait before next iteration
            print(f"\n⏳ Waiting {check_interval}s...")
            time.sleep(check_interval)
        
        print(f"\n{'='*60}")
        print("✅ Autonomous run complete!")
        print(f"{'='*60}")
        print(f"\n📊 Final Stats:")
        print(f"   Coverage: {state['current_coverage']} instructions")
        print(f"   Iterations: {state['iterations']}")
        print(f"   Corpus: {state['corpus_size']} sequences")


def main():
    """Example usage."""
    import os
    
    # Check for API key
    if not os.getenv("ANTHROPIC_API_KEY"):
        print("❌ Please set ANTHROPIC_API_KEY environment variable")
        print("   export ANTHROPIC_API_KEY=your_key_here")
        return
    
    print("🤖 LangGraph Agent for Echidna MCP")
    print("=" * 60)
    
    agent = EchidnaLangGraphAgent()
    
    # Test connectivity
    try:
        status = agent._call_mcp_tool("status")
        if "error" in status:
            print("❌ Cannot connect to Echidna MCP server")
            print("   Make sure Echidna is running with --server 8080")
            return
        print("✅ Connected to Echidna MCP server\n")
    except Exception as e:
        print(f"❌ Error: {e}")
        return
    
    # Run autonomous agent
    duration = int(input("Enter duration in minutes (default 10): ") or "10")
    interval = int(input("Enter check interval in seconds (default 60): ") or "60")
    
    agent.run_autonomous(duration_minutes=duration, check_interval=interval)


if __name__ == "__main__":
    main()

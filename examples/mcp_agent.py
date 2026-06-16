#!/usr/bin/env python3
"""
LangGraph agent for Echidna MCP integration.

Connects to a running Echidna campaign via its MCP server and autonomously
guides the fuzzer: analyzes coverage, injects targeted transactions, and
resets priorities when coverage stagnates.

Requirements:
    pip install langchain-anthropic langgraph httpx

Usage:
    # Start Echidna with MCP enabled:
    echidna MyContract.sol --server 8080 --format text

    # Run the agent:
    ANTHROPIC_API_KEY=... python examples/mcp_agent.py
"""

import os
import time
import httpx
from typing import TypedDict, List
from langchain_anthropic import ChatAnthropic
from langgraph.graph import StateGraph, END


# ---------------------------------------------------------------------------
# MCP client
# ---------------------------------------------------------------------------

def call_tool(tool: str, args: dict = None) -> str:
    """Call an MCP tool and return the text response."""
    payload = {
        "jsonrpc": "2.0", "id": 1,
        "method": "tools/call",
        "params": {"name": tool, "arguments": args or {}},
    }
    resp = httpx.post("http://localhost:8080/mcp", json=payload, timeout=30)
    result = resp.json().get("result", {})
    return result.get("content", [{}])[0].get("text", "")


# ---------------------------------------------------------------------------
# Agent state
# ---------------------------------------------------------------------------

class State(TypedDict):
    coverage: int
    iterations: int
    stagnation: int


def parse_int(text: str, key: str) -> int:
    for line in text.splitlines():
        if key in line:
            try:
                return int(line.split(":")[-1].strip().split()[0].replace(",", ""))
            except ValueError:
                pass
    return 0


# ---------------------------------------------------------------------------
# Graph nodes
# ---------------------------------------------------------------------------

def observe(state: State) -> State:
    """Read current campaign status."""
    status = call_tool("status")
    coverage = parse_int(status, "Coverage")
    iterations = parse_int(status, "Iterations")
    stagnation = state["stagnation"] + 1 if coverage == state["coverage"] else 0
    print(f"  coverage={coverage}  iterations={iterations}  stagnation={stagnation}")
    return {"coverage": coverage, "iterations": iterations, "stagnation": stagnation}


def inject(state: State) -> State:
    """Ask the LLM to suggest targeted transactions and inject them."""
    llm = ChatAnthropic(model="claude-sonnet-4-5", temperature=0.7)
    coverage_report = call_tool("show_coverage")

    prompt = (
        "You are a smart-contract security expert helping an Echidna fuzzer.\n"
        "The coverage report is:\n\n"
        f"{coverage_report[:2000]}\n\n"
        "Suggest 3 Solidity function calls that would increase coverage.\n"
        "Reply with one call per line, e.g.: transfer(0xABCD..., 100)\n"
    )
    suggestions = llm.invoke(prompt).content.strip().splitlines()

    for tx in suggestions[:3]:
        tx = tx.strip()
        if tx and "(" in tx and not tx.startswith("#"):
            print(f"  injecting: {tx}")
            call_tool("inject_fuzz_transactions", {"transactions": tx})

    return {**state, "stagnation": 0}


def reset(state: State) -> State:
    """Clear function priorities to encourage exploration."""
    print("  clearing priorities")
    call_tool("clear_fuzz_priorities")
    return state


def route(state: State) -> str:
    if state["stagnation"] >= 3:
        return "inject"
    if state["iterations"] > 0 and state["iterations"] % 100_000 == 0:
        return "reset"
    return END


# ---------------------------------------------------------------------------
# Build and run
# ---------------------------------------------------------------------------

def build_graph() -> StateGraph:
    g = StateGraph(State)
    g.add_node("observe", observe)
    g.add_node("inject", inject)
    g.add_node("reset", reset)
    g.set_entry_point("observe")
    g.add_conditional_edges("observe", route, {"inject": "inject", "reset": "reset", END: END})
    g.add_edge("inject", END)
    g.add_edge("reset", END)
    return g.compile()


def main():
    if not os.getenv("ANTHROPIC_API_KEY"):
        print("Set ANTHROPIC_API_KEY before running.")
        return

    # Verify connectivity
    try:
        status = call_tool("status")
        if not status:
            raise RuntimeError("empty response")
        print("Connected to Echidna MCP server.")
    except Exception as e:
        print(f"Cannot reach MCP server: {e}")
        print("Start Echidna with:  echidna MyContract.sol --server 8080 --format text")
        return

    graph = build_graph()
    state: State = {"coverage": 0, "iterations": 0, "stagnation": 0}

    duration = int(input("Duration in minutes [10]: ") or 10)
    interval = int(input("Check interval in seconds [60]: ") or 60)
    end_time = time.time() + duration * 60

    step = 0
    while time.time() < end_time:
        step += 1
        print(f"\n--- step {step} ---")
        state = graph.invoke(state)
        time.sleep(interval)

    print(f"\nDone. Final coverage: {state['coverage']}  iterations: {state['iterations']}")


if __name__ == "__main__":
    main()

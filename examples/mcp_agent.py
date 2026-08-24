#!/usr/bin/env python3
"""
LangGraph agent for Echidna's MCP server.

Connects to a running campaign, watches what the fuzzer is reaching, and when
coverage stalls asks Claude for call sequences to aim it at. A demonstration of
what the server is for rather than a tuned strategy: the interesting part is
that `status` and `show_coverage` are enough for a model to decide what to try
next, and `inject_fuzz_transactions` is enough to act on it.

Requirements:
    pip install langchain-anthropic langgraph httpx

Usage:
    # Start Echidna with the MCP server. --format text is required: the
    # interactive TUI otherwise owns the terminal.
    echidna MyContract.sol --server 8080 --format text

    # Run the agent:
    ANTHROPIC_API_KEY=... python examples/mcp_agent.py
"""

import json
import os
import re
import time
from typing import TypedDict

import httpx
from langchain_anthropic import ChatAnthropic
from langgraph.graph import END, StateGraph

MCP_URL = os.environ.get("ECHIDNA_MCP_URL", "http://127.0.0.1:8080/mcp")
PROTOCOL_VERSION = "2025-06-18"

# How long the campaign may go without finding coverage before the agent steps
# in, and how often it looks.
STALL_SECONDS = 60
INTERVAL_SECONDS = 30


# ---------------------------------------------------------------------------
# MCP client
# ---------------------------------------------------------------------------

def _rpc(method: str, params: dict | None = None, request_id: int | None = 1):
    body = {"jsonrpc": "2.0", "method": method}
    if request_id is not None:
        body["id"] = request_id
    if params is not None:
        body["params"] = params
    headers = {
        "Content-Type": "application/json",
        "Accept": "application/json, text/event-stream",
    }
    if method != "initialize":
        headers["MCP-Protocol-Version"] = PROTOCOL_VERSION
    response = httpx.post(MCP_URL, json=body, headers=headers, timeout=60)
    response.raise_for_status()
    return response.json() if request_id is not None else None


def connect() -> str:
    """Run the MCP handshake and return the server's name."""
    result = _rpc("initialize", {
        "protocolVersion": PROTOCOL_VERSION,
        "capabilities": {},
        "clientInfo": {"name": "echidna-mcp-agent", "version": "0"},
    }, request_id=0)["result"]
    _rpc("notifications/initialized", request_id=None)
    return result["serverInfo"]["name"]


def call_tool(name: str, arguments: dict | None = None) -> str:
    """Call a tool and return its report, raising if the tool could not answer."""
    result = _rpc("tools/call", {"name": name, "arguments": arguments or {}})["result"]
    text = "".join(part.get("text", "") for part in result.get("content", []))
    if result.get("isError"):
        raise RuntimeError(f"{name}: {text}")
    return text


# ---------------------------------------------------------------------------
# Graph
# ---------------------------------------------------------------------------

class State(TypedDict):
    contract: str
    iterations: int
    coverage: int
    stalled_for: int


def observe(state: State) -> State:
    """Read how the campaign is going."""
    status = json.loads(call_tool("status"))
    stalled_for = status["time_since_last_coverage_sec"] or 0
    print(f"  coverage={status['coverage_points']}  "
          f"iterations={status['iterations']}  "
          f"last coverage {stalled_for}s ago "
          f"({', '.join(status['recent_covered_functions'][:3]) or 'nothing yet'})")
    return {
        **state,
        "iterations": status["iterations"],
        "coverage": status["coverage_points"],
        "stalled_for": stalled_for,
    }


def inject(state: State) -> State:
    """Ask Claude which orderings to try, and point the fuzzer at them."""
    # temperature is not accepted on current models; steer with the prompt.
    llm = ChatAnthropic(model="claude-opus-4-8", max_tokens=1024)

    abi = call_tool("target")
    coverage = call_tool("show_coverage", {"contract": state["contract"]})

    answer = llm.invoke(
        "You are helping an Echidna fuzzing campaign reach code it has not "
        "reached. It has found no new coverage for a while.\n\n"
        f"The contract under test exposes:\n{abi}\n\n"
        f"Its coverage so far — lines marked * were executed, r reverted, "
        f"o ran out of gas, e errored, blank was never reached:\n"
        f"{coverage[:8000]}\n\n"
        "Reply with up to 3 call sequences that would reach unmarked lines, one "
        "per line and nothing else. Separate the calls in a sequence with ';', "
        "and write '?' for any argument the fuzzer should choose. For example:\n"
        "approve(0x10, ?); transferFrom(?, ?, 100)\n"
    ).content

    # Anything with a call in it; the model's prose, if any, has no parentheses.
    sequences = [line.strip() for line in answer.splitlines()
                 if re.search(r"\w+\s*\(", line)]

    # Drop what was injected last time first, so the fuzzer is not left
    # splitting its budget across every ordering the agent has ever suggested.
    call_tool("clear_fuzz_priorities")
    for sequence in sequences[:3]:
        try:
            print(f"  injecting: {sequence}")
            print(f"    {call_tool('inject_fuzz_transactions', {'transactions': sequence})}")
        except RuntimeError as complaint:
            print(f"    rejected: {complaint}")

    return state


def route(state: State) -> str:
    return "inject" if state["stalled_for"] >= STALL_SECONDS else END


def build_graph():
    graph = StateGraph(State)
    graph.add_node("observe", observe)
    graph.add_node("inject", inject)
    graph.set_entry_point("observe")
    graph.add_conditional_edges("observe", route, {"inject": "inject", END: END})
    graph.add_edge("inject", END)
    return graph.compile()


def main() -> None:
    if not os.getenv("ANTHROPIC_API_KEY"):
        print("Set ANTHROPIC_API_KEY before running.")
        return

    try:
        print(f"Connected to {connect()} at {MCP_URL}.")
    except Exception as unreachable:
        print(f"Cannot reach the MCP server: {unreachable}")
        print("Start Echidna with:  echidna MyContract.sol --server 8080 --format text")
        return

    # "Contract: path/to/File.sol:Name" — the name is what show_coverage wants.
    contract = call_tool("target").splitlines()[0].rsplit(":", 1)[1].strip()
    print(f"Watching {contract}. Stepping in after {STALL_SECONDS}s without coverage.")

    graph = build_graph()
    state: State = {"contract": contract, "iterations": 0, "coverage": 0, "stalled_for": 0}

    step = 0
    while True:
        step += 1
        print(f"\n--- step {step} ---")
        state = graph.invoke(state)
        time.sleep(INTERVAL_SECONDS)


if __name__ == "__main__":
    main()

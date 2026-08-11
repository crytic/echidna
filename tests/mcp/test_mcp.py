"""
Tool/semantic integration tests for the Echidna MCP server.

Verifies the core MCP tools return sensible results against a live Echidna
campaign: status, inject_fuzz_transactions, show_coverage, clear_fuzz_priorities.

Originally contributed by Dani Tradito (@datradito) in crytic/echidna#1509;
adapted here to share the session-scoped `echidna_server` fixture in conftest.py.

Run with:
    pytest tests/mcp/test_mcp.py
"""

import httpx


def _call_tool(url: str, tool: str, args: dict = None) -> dict:
    payload = {
        "jsonrpc": "2.0", "id": 1,
        "method": "tools/call",
        "params": {"name": tool, "arguments": args or {}},
    }
    resp = httpx.post(url, json=payload, timeout=30)
    resp.raise_for_status()
    return resp.json().get("result", {})


def _text(result: dict) -> str:
    return result.get("content", [{}])[0].get("text", "")


def test_status(echidna_server):
    """status tool returns campaign metrics."""
    result = _call_tool(echidna_server["url"], "status")
    text = _text(result)
    assert text, "status returned empty response"
    assert "corpus" in text.lower() or "coverage" in text.lower(), (
        f"Unexpected status text: {text[:200]}"
    )


def test_inject_transactions(echidna_server):
    """inject_fuzz_transactions accepts a call sequence and confirms injection."""
    result = _call_tool(echidna_server["url"], "inject_fuzz_transactions", {
        "transactions": (
            "transfer(0x1111111111111111111111111111111111111111, 100);"
            "approve(0x2222222222222222222222222222222222222222, 50)"
        )
    })
    text = _text(result)
    assert text, "inject_fuzz_transactions returned empty response"
    assert "requested" in text.lower() or "inject" in text.lower() or "fuzz" in text.lower(), (
        f"Unexpected inject response: {text[:200]}"
    )


def test_show_coverage(echidna_server):
    """show_coverage returns a non-empty coverage report."""
    result = _call_tool(echidna_server["url"], "show_coverage")
    text = _text(result)
    assert isinstance(text, str), "show_coverage response is not a string"
    assert len(text) > 0, "show_coverage returned empty report"


def test_clear_priorities_and_status(echidna_server):
    """clear_fuzz_priorities succeeds, then status is still reachable."""
    clear = _call_tool(echidna_server["url"], "clear_fuzz_priorities")
    clear_text = _text(clear)
    assert clear_text, "clear_fuzz_priorities returned empty response"
    assert "requested" in clear_text.lower() or "clear" in clear_text.lower(), (
        f"Unexpected clear response: {clear_text[:200]}"
    )

    # Status should still work after clearing
    status = _call_tool(echidna_server["url"], "status")
    assert _text(status), "status failed after clear_fuzz_priorities"

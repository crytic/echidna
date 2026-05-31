"""
Basic integration tests for the Echidna MCP server.

Tests the core workflow: inject transactions, check coverage,
reset priorities, and verify status.

Run with:
    pytest tests/mcp/test_mcp.py
"""

import pytest
import socket
import time
import os
import subprocess
import httpx


MCP_PORT = 8080
MCP_URL = f"http://localhost:{MCP_PORT}/mcp"
CONTRACT = "tests/mcp/contracts/EchidnaMCPTest.sol"


# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

def _call_tool(tool: str, args: dict = None) -> dict:
    payload = {
        "jsonrpc": "2.0", "id": 1,
        "method": "tools/call",
        "params": {"name": tool, "arguments": args or {}},
    }
    resp = httpx.post(MCP_URL, json=payload, timeout=30)
    resp.raise_for_status()
    return resp.json().get("result", {})


def _text(result: dict) -> str:
    return result.get("content", [{}])[0].get("text", "")


@pytest.fixture(scope="module")
def echidna(tmp_path_factory):
    """Start an Echidna campaign with the MCP server for the whole test module."""
    corpus_dir = tmp_path_factory.mktemp("corpus")
    env = os.environ.copy()
    env["PATH"] = os.path.expanduser("~/.local/bin") + ":" + env.get("PATH", "")

    cmd = [
        "echidna", CONTRACT,
        "--contract", "EchidnaMCPTest",
        "--server", str(MCP_PORT),
        "--format", "text",
        "--test-limit", "1000000000",
        "--corpus-dir", str(corpus_dir),
    ]
    proc = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
                            text=True, env=env)

    # Wait up to 10 s for the server to accept connections
    deadline = time.time() + 10
    while time.time() < deadline:
        try:
            with socket.create_connection(("localhost", MCP_PORT), timeout=1):
                break
        except OSError:
            time.sleep(0.5)
    else:
        proc.terminate()
        out, err = proc.communicate(timeout=5)
        raise RuntimeError(
            f"Echidna MCP server did not start.\nstdout: {out}\nstderr: {err}"
        )

    yield proc

    proc.terminate()
    try:
        proc.communicate(timeout=5)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait()


# ---------------------------------------------------------------------------
# Tests
# ---------------------------------------------------------------------------

def test_status(echidna):
    """status tool returns campaign metrics."""
    result = _call_tool("status")
    text = _text(result)
    assert text, "status returned empty response"
    assert "corpus" in text.lower() or "coverage" in text.lower(), (
        f"Unexpected status text: {text[:200]}"
    )


def test_inject_transactions(echidna):
    """inject_fuzz_transactions accepts a call sequence and confirms injection."""
    result = _call_tool("inject_fuzz_transactions", {
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


def test_show_coverage(echidna):
    """show_coverage returns a non-empty coverage report."""
    result = _call_tool("show_coverage")
    text = _text(result)
    assert isinstance(text, str), "show_coverage response is not a string"
    assert len(text) > 0, "show_coverage returned empty report"


def test_clear_priorities_and_status(echidna):
    """clear_fuzz_priorities succeeds, then status is still reachable."""
    clear = _call_tool("clear_fuzz_priorities")
    clear_text = _text(clear)
    assert clear_text, "clear_fuzz_priorities returned empty response"
    assert "requested" in clear_text.lower() or "clear" in clear_text.lower(), (
        f"Unexpected clear response: {clear_text[:200]}"
    )

    # Status should still work after clearing
    status = _call_tool("status")
    assert _text(status), "status failed after clear_fuzz_priorities"

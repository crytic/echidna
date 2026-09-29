"""
Minimal, dependency-light MCP-over-HTTP client helpers shared by every module
in this suite.

These intentionally do NOT use a high-level MCP SDK: they speak the wire
protocol directly so the tests can assert on exact status codes, headers and
bodies — the things real clients (Codex's rmcp, Anthropic's MCP client) are
strict about and that a lenient `httpx.post(...).json()` would silently hide.
The SDK does get exercised, as a client rather than as a helper, in
test_mcp_claude.py.
"""

import time

import httpx

PROTOCOL_VERSION = "2025-06-18"
SUPPORTED_VERSIONS = ("2025-06-18", "2025-03-26", "2024-11-05")


class ToolError(Exception):
    """A tool answered with an MCP error result instead of a report."""


def rpc(url, method, params=None, id=1, protocol_version=PROTOCOL_VERSION,
        accept="application/json, text/event-stream", timeout=30):
    """POST a single JSON-RPC message; return the raw httpx.Response (never parsed)."""
    body = {"jsonrpc": "2.0", "method": method}
    if id is not None:
        body["id"] = id
    if params is not None:
        body["params"] = params
    headers = {"Content-Type": "application/json", "Accept": accept}
    # Per the spec the MCP-Protocol-Version header is sent on requests *after*
    # initialization, not on `initialize` itself.
    if protocol_version is not None and method != "initialize":
        headers["MCP-Protocol-Version"] = protocol_version
    return httpx.post(url, json=body, headers=headers, timeout=timeout)


def http_get(url, accept="text/event-stream", timeout=10):
    """GET the MCP endpoint (clients use this to open the server->client SSE stream)."""
    return httpx.get(url, headers={"Accept": accept}, timeout=timeout)


def handshake(url, protocol_version=PROTOCOL_VERSION):
    """Run the client half of the MCP handshake; return the InitializeResult dict.

    initialize -> (read result) -> notifications/initialized
    """
    resp = rpc(url, "initialize", {
        "protocolVersion": protocol_version,
        "capabilities": {},
        "clientInfo": {"name": "echidna-mcp-tests", "version": "0"},
    }, id=0, protocol_version=None)
    resp.raise_for_status()
    result = resp.json()["result"]
    negotiated = result.get("protocolVersion", protocol_version)
    rpc(url, "notifications/initialized", id=None, protocol_version=negotiated)
    return result


def call_tool(url, name, arguments=None, protocol_version=PROTOCOL_VERSION,
              timeout=60):
    """Call a tool and return its report.

    A tool that cannot do what it was asked answers with an MCP error result
    rather than with a report, so that becomes a ToolError here: a test that
    did not expect one fails at the call rather than on some later assertion
    about the text. Tests that do expect one use `pytest.raises(ToolError)`.
    """
    resp = rpc(url, "tools/call", {"name": name, "arguments": arguments or {}},
               id=2, protocol_version=protocol_version, timeout=timeout)
    resp.raise_for_status()
    body = resp.json()
    if "error" in body:
        raise ToolError(f"{name}: JSON-RPC error {body['error']}")
    result = body["result"]
    text = "".join(part.get("text", "") for part in result.get("content", []))
    if result.get("isError"):
        raise ToolError(f"{name}: {text}")
    return text


def eventually(read, timeout=60, interval=0.5):
    """Poll `read` until it answers with something truthy, and return that.

    Nothing a client asks for takes effect at once: a worker only looks at the
    bus between sequences, and what it does then only shows up in `status` on
    the next call. Returns None if the timeout runs out, so the caller can say
    what it was waiting for.
    """
    deadline = time.time() + timeout
    while True:
        answer = read()
        if answer:
            return answer
        if time.time() >= deadline:
            return None
        time.sleep(interval)

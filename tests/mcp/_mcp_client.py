"""
Minimal, dependency-light MCP-over-HTTP client helpers shared by the
conformance and Codex-replay test layers.

These intentionally do NOT use a high-level MCP SDK: they speak the wire
protocol directly so the tests can assert on exact status codes, headers and
bodies — the things real clients (Codex's rmcp, Anthropic's MCP client) are
strict about and that a lenient `httpx.post(...).json()` would silently hide.
"""

import httpx

PROTOCOL_VERSION = "2025-06-18"
SUPPORTED_VERSIONS = ("2025-06-18", "2025-03-26", "2024-11-05")


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


def call_tool(url, name, arguments=None, protocol_version=PROTOCOL_VERSION):
    """tools/call helper that returns the parsed JSON-RPC `result`."""
    resp = rpc(url, "tools/call",
               {"name": name, "arguments": arguments or {}},
               id=2, protocol_version=protocol_version)
    resp.raise_for_status()
    return resp.json().get("result", {})

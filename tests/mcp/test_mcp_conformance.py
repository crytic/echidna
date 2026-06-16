"""
Wire-protocol conformance tests for the Echidna MCP server.

This is the regression guard for "real MCP clients (Claude, Codex) can still
connect." It asserts exact transport behaviour that a lenient
`httpx.post(...).json()` hides but strict clients depend on -- in particular
the behaviours that broke Codex's rmcp client and were fixed in
haskell-mcp-server:

  * a notification (no `id`) -> 202 Accepted with NO body
  * GET on the endpoint (an SSE-stream request) -> 405, never 200/JSON
  * initialize -> a negotiated protocolVersion + serverInfo + capabilities

Run with:
    pytest tests/mcp/test_mcp_conformance.py
"""

from _mcp_client import PROTOCOL_VERSION, SUPPORTED_VERSIONS, rpc, http_get, handshake


def test_initialize_negotiates_protocol_version(echidna_server):
    url = echidna_server["url"]
    resp = rpc(url, "initialize", {
        "protocolVersion": PROTOCOL_VERSION,
        "capabilities": {},
        "clientInfo": {"name": "conformance", "version": "0"},
    }, id=0, protocol_version=None)
    assert resp.status_code == 200, f"initialize must return 200, got {resp.status_code}"
    result = resp.json()["result"]
    assert result.get("protocolVersion") in SUPPORTED_VERSIONS, (
        f"unexpected negotiated protocolVersion: {result.get('protocolVersion')}"
    )
    assert "serverInfo" in result, "initialize result missing serverInfo"
    assert "capabilities" in result, "initialize result missing capabilities"


def test_notification_returns_202_no_body(echidna_server):
    """The Codex/rmcp regression guard.

    A notification (no `id`) MUST get HTTP 202 with an empty body. Returning
    `200 {}` makes rmcp fail with
    'data did not match any variant of untagged enum JsonRpcMessage' and drop
    the whole server -- the exact bug this suite exists to prevent.
    """
    url = echidna_server["url"]
    resp = rpc(url, "notifications/initialized", id=None)
    assert resp.status_code == 202, (
        f"notifications/initialized must return 202, got {resp.status_code} "
        f"with body {resp.content!r}"
    )
    assert resp.content == b"", (
        f"a 202 response must have an empty body, got {resp.content!r}"
    )


def test_get_is_sse_or_405_never_plain_json(echidna_server):
    """A GET asking for an SSE stream must be 405 (no server stream) or a real
    text/event-stream -- never 200 application/json, which strict clients try
    to read as SSE and choke on."""
    url = echidna_server["url"]
    resp = http_get(url, accept="text/event-stream")
    ctype = resp.headers.get("content-type", "")
    assert resp.status_code == 405 or "text/event-stream" in ctype, (
        f"GET must be 405 or an SSE stream, got {resp.status_code} ({ctype})"
    )


def test_tools_list_after_handshake(echidna_server):
    url = echidna_server["url"]
    handshake(url)
    resp = rpc(url, "tools/list", id=3)
    assert resp.status_code == 200
    tools = resp.json()["result"]["tools"]
    names = {t["name"] for t in tools}
    assert {"status", "inject_fuzz_transactions", "clear_fuzz_priorities"} <= names, (
        f"missing expected tools, got {sorted(names)}"
    )
    for t in tools:
        assert "inputSchema" in t, f"tool {t.get('name')} missing inputSchema"


def test_unknown_method_returns_jsonrpc_error(echidna_server):
    url = echidna_server["url"]
    resp = rpc(url, "this/method/does/not/exist", id=4)
    body = resp.json()
    assert "error" in body, f"unknown method should yield a JSON-RPC error, got {body}"


def test_tool_call_roundtrip(echidna_server):
    """A full handshake + tools/call returns content (what an agent actually does)."""
    url = echidna_server["url"]
    handshake(url)
    resp = rpc(url, "tools/call", {"name": "status", "arguments": {}}, id=5)
    assert resp.status_code == 200
    result = resp.json()["result"]
    content = result.get("content", [])
    assert content and content[0].get("text"), f"status returned no content: {result}"

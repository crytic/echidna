"""
Codex (rmcp) client-compatibility test.

Codex connects to MCP servers with the Rust `rmcp` StreamableHttpClientTransport,
which is strict about the wire protocol. We can't run rmcp from Python, so this
module *replays Codex's exact observed handshake* against the live server and
asserts the server satisfies rmcp's expectations at each step. A regression that
would make Codex drop the server fails here -- without needing Codex installed
or authenticated.

The sequence and failure modes below were reverse-engineered from a real
`codex exec` run against an Echidna MCP server.

For an end-to-end smoke with the real binary (needs Codex auth, so it is NOT in
CI): point Codex's `~/.codex/config.toml` at the running campaign and run
`codex mcp get echidna`.

Run with:
    pytest tests/mcp/test_mcp_codex.py
"""

from _mcp_client import PROTOCOL_VERSION, SUPPORTED_VERSIONS, rpc, http_get


def test_codex_rmcp_handshake_replay(echidna_server):
    url = echidna_server["url"]

    # 1. initialize -- rmcp sends this first (Accept: application/json, text/event-stream).
    init = rpc(url, "initialize", {
        "protocolVersion": PROTOCOL_VERSION,
        "capabilities": {"roots": {"listChanged": True}},
        "clientInfo": {"name": "codex", "version": "0"},
    }, id=0, protocol_version=None)
    assert init.status_code == 200, f"initialize: HTTP {init.status_code}"
    init_result = init.json()["result"]
    assert init_result.get("protocolVersion") in SUPPORTED_VERSIONS, (
        f"protocol negotiation: {init_result.get('protocolVersion')}"
    )
    negotiated = init_result["protocolVersion"]

    # 2. notifications/initialized -- rmcp DIES here if the server answers `200 {}`
    #    ("data did not match any variant of untagged enum JsonRpcMessage").
    note = rpc(url, "notifications/initialized", id=None, protocol_version=negotiated)
    assert note.status_code == 202 and note.content == b"", (
        "Codex's rmcp client requires 202 + empty body for notifications; "
        f"got {note.status_code} {note.content!r}"
    )

    # 3. GET the endpoint to open the server->client SSE stream. rmcp tolerates
    #    405 ("no server stream"); a 200 application/json discovery blob breaks it.
    stream = http_get(url, accept="text/event-stream")
    ctype = stream.headers.get("content-type", "")
    assert stream.status_code == 405 or "text/event-stream" in ctype, (
        f"GET stream must be 405 or SSE; got {stream.status_code} ({ctype})"
    )

    # 4. tools/list -- only reached if the handshake survived.
    tools = rpc(url, "tools/list", id=1, protocol_version=negotiated)
    assert tools.status_code == 200, f"tools/list: HTTP {tools.status_code}"
    names = {t["name"] for t in tools.json()["result"]["tools"]}
    assert "status" in names, f"tools not exposed to Codex: {sorted(names)}"

    # 5. tools/call -- the agent's first real action.
    call = rpc(url, "tools/call", {"name": "status", "arguments": {}}, id=2,
               protocol_version=negotiated)
    assert call.status_code == 200, f"tools/call: HTTP {call.status_code}"
    assert call.json()["result"]["content"][0]["text"], "status returned no content"

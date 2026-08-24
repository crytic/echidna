"""
Claude (Anthropic MCP client) compatibility test.

Anthropic's clients speak MCP via the reference `mcp` SDK. This test drives the
server with that SDK's Streamable-HTTP client through a full, real session
(initialize -> list_tools -> call_tool). If the server's transport is
non-conformant, the SDK -- like Claude -- fails to establish the session, so
this is a faithful "would Claude break?" check.

No API key is required: it exercises the MCP transport, not the model. (An
end-to-end run with a live model is the LangGraph demo in examples/mcp_agent.py,
which needs ANTHROPIC_API_KEY and is not part of CI.)

Requires the `mcp` SDK and pytest-asyncio (see requirements-test.txt); the test
skips itself if neither is installed.

Run with:
    pip install -r tests/mcp/requirements-test.txt
    pytest tests/mcp/test_mcp_claude.py
"""

import pytest

transport = pytest.importorskip(
    "mcp.client.streamable_http",
    reason="install the `mcp` SDK to run the Claude client test",
)

# The SDK's 2.0 renamed the transport helper, stopped yielding a third stream
# alongside the two, and moved its result models to snake_case. Rather than
# straddle both spellings, follow the current major and say so.
if not hasattr(transport, "streamable_http_client"):
    pytest.skip(
        "needs the `mcp` SDK 2.x; see tests/mcp/requirements-test.txt",
        allow_module_level=True,
    )

from mcp import ClientSession  # noqa: E402
from mcp.client.streamable_http import streamable_http_client  # noqa: E402


@pytest.mark.asyncio
async def test_claude_mcp_sdk_session(echidna_server):
    """A real MCP SDK client completes a session and calls a tool."""
    async with streamable_http_client(echidna_server["url"]) as (read, write):
        async with ClientSession(read, write) as session:
            init = await session.initialize()
            assert init.server_info is not None, "initialize returned no serverInfo"

            tools = await session.list_tools()
            names = {t.name for t in tools.tools}
            assert "status" in names, (
                f"tools not visible to the MCP SDK client: {sorted(names)}"
            )

            result = await session.call_tool("status", {})
            assert not result.is_error, f"status tool reported an error: {result}"
            assert result.content, "status returned no content to the MCP SDK client"
            text = getattr(result.content[0], "text", "")
            assert text, f"empty status content: {result.content}"

"""
Shared pytest fixtures for the out-of-process MCP test suite.

A single session-scoped Echidna campaign (with the MCP server enabled) is
started once and reused by every test module: the tool/semantic tests
(test_mcp.py), the wire-protocol conformance tests (test_mcp_conformance.py),
and the client-simulation tests (test_mcp_claude.py, test_mcp_codex.py).

The Echidna launch/wait logic was extracted from the original test_mcp.py
contributed by Dani Tradito (@datradito) in crytic/echidna#1509.
"""

import os
import subprocess
import time

import pytest

from _mcp_client import PROTOCOL_VERSION, rpc

MCP_PORT = int(os.environ.get("ECHIDNA_MCP_PORT", "8080"))
MCP_URL = f"http://127.0.0.1:{MCP_PORT}/mcp"
ECHIDNA_BIN = os.environ.get("ECHIDNA_BIN", "echidna")
CONTRACT = os.path.join(os.path.dirname(__file__), "contracts", "EchidnaMCPTest.sol")


def _server_ready() -> bool:
    """True once the MCP server answers a real `initialize` request."""
    try:
        resp = rpc(MCP_URL, "initialize", {
            "protocolVersion": PROTOCOL_VERSION,
            "capabilities": {},
            "clientInfo": {"name": "readiness-probe", "version": "0"},
        }, id=0, protocol_version=None, timeout=2)
        return resp.status_code == 200 and "result" in resp.json()
    except Exception:
        return False


@pytest.fixture(scope="session")
def echidna_server(tmp_path_factory):
    """Start an Echidna campaign with the MCP server for the whole test session.

    If ECHIDNA_MCP_URL is set, use that already-running server instead of
    starting one (handy for pointing the suite at a live campaign or, in CI, a
    deliberately broken server to prove the conformance tests have teeth).
    """
    external = os.environ.get("ECHIDNA_MCP_URL")
    if external:
        yield {"url": external, "port": None, "protocol_version": PROTOCOL_VERSION}
        return

    corpus_dir = tmp_path_factory.mktemp("corpus")

    env = os.environ.copy()
    env["PATH"] = os.path.expanduser("~/.local/bin") + os.pathsep + env.get("PATH", "")
    # Echidna shells out to solc/crytic-compile; an active VIRTUAL_ENV breaks that.
    env.pop("VIRTUAL_ENV", None)

    cmd = [
        ECHIDNA_BIN, CONTRACT,
        "--contract", "EchidnaMCPTest",
        "--server", str(MCP_PORT),
        "--format", "text",
        "--test-limit", "1000000000",
        "--corpus-dir", str(corpus_dir),
    ]
    proc = subprocess.Popen(
        cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, env=env
    )

    # Wait (compiling the contract can take a while) for the server to accept a
    # real MCP initialize, not merely an open socket.
    deadline = time.time() + 120
    while time.time() < deadline:
        if proc.poll() is not None:
            out, err = proc.communicate()
            raise RuntimeError(f"Echidna exited early.\nstdout:\n{out}\nstderr:\n{err}")
        if _server_ready():
            break
        time.sleep(0.5)
    else:
        proc.terminate()
        out, err = proc.communicate(timeout=5)
        raise RuntimeError(f"MCP server did not become ready.\nstdout:\n{out}\nstderr:\n{err}")

    yield {"url": MCP_URL, "port": MCP_PORT, "protocol_version": PROTOCOL_VERSION}

    proc.terminate()
    try:
        proc.communicate(timeout=5)
    except subprocess.TimeoutExpired:
        proc.kill()
        proc.wait()

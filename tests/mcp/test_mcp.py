"""
Tool tests for the Echidna MCP server: what the nine tools answer when they are
driven against a live campaign, and what they say when they cannot answer.

The transport is somebody else's problem here (test_mcp_conformance.py), as is
the promise that `execute_sequence` leaves the campaign alone, which the
Haskell suite pins down from the inside. What is left is the interface an agent
sees.

Originally contributed by Dani Tradito (@datradito) in crytic/echidna#1509;
adapted here to the session-scoped campaign in conftest.py.

Run with:
    pytest tests/mcp/test_mcp.py
"""

import json
import os
import re

import pytest

from _mcp_client import ToolError, call_tool, eventually, rpc


@pytest.fixture(autouse=True)
def fixture_campaign(echidna_server, mcp_url):
    """These tests are about the campaign conftest.py starts, and they leave it
    as they found it.

    Injection and sampling are campaign-wide and outlive the test that turned
    them on, so undoing them here is what keeps the tests independent of the
    order they run in.
    """
    if not echidna_server["local"]:
        pytest.skip("ECHIDNA_MCP_URL points at a campaign this module knows nothing about")
    yield
    call_tool(mcp_url, "clear_fuzz_priorities")
    call_tool(mcp_url, "sample", {"function": "off"})


def status(url) -> dict:
    return json.loads(call_tool(url, "status"))


def test_every_tool_is_advertised(mcp_url):
    """The tools an agent can reach, which examples/README.md documents."""
    listed = rpc(mcp_url, "tools/list", id=3).json()["result"]["tools"]

    assert {tool["name"] for tool in listed} == {
        "status", "target", "show_coverage", "dump_lcov", "reload_corpus",
        "inject_fuzz_transactions", "clear_fuzz_priorities", "execute_sequence",
        "sample",
    }
    for tool in listed:
        assert tool["description"], f"{tool['name']} is undescribed"


def test_status_reports_campaign_metrics(mcp_url):
    report = status(mcp_url)

    # The two echidna_ properties of the fixture contract, both of which hold.
    assert report["tests_total"] == 2
    assert report["tests_failed"] == 0
    assert report["optimization_values"] == []

    assert eventually(lambda: status(mcp_url)["iterations"] > 0), \
        "the campaign never reported an iteration"
    assert eventually(lambda: status(mcp_url)["coverage_points"] > 0), \
        "the campaign never reported any coverage"
    assert eventually(lambda: status(mcp_url)["recent_covered_functions"]), \
        "no function was credited with finding coverage"


def test_target_lists_the_abi(mcp_url):
    report = call_tool(mcp_url, "target")

    assert "EchidnaMCPTest" in report
    for signature in ("transferTokens(address,uint256)",
                      "approveSpender(address,uint256)",
                      "mintTokens(address,uint256)"):
        assert signature in report, f"{signature} missing from:\n{report}"


def test_show_coverage_marks_up_the_source(mcp_url):
    report = call_tool(mcp_url, "show_coverage", {"contract": "EchidnaMCPTest"})

    assert "EchidnaMCPTest.sol" in report
    assert "contract EchidnaMCPTest {" in report, "the source is not in the report"
    # " 12 | *r  | contract EchidnaMCPTest {": line number, markers, source.
    assert re.search(r"^\s*\d+ \| *\*", report, re.M), \
        f"nothing is marked as executed:\n{report[:2000]}"


def test_show_coverage_needs_a_contract_that_exists(mcp_url):
    with pytest.raises(ToolError, match="required"):
        call_tool(mcp_url, "show_coverage")

    with pytest.raises(ToolError, match="No contract by that name"):
        call_tool(mcp_url, "show_coverage", {"contract": "NotAContract"})


def test_dump_lcov_writes_a_file(echidna_server, mcp_url):
    report = call_tool(mcp_url, "dump_lcov")

    written = re.fullmatch(r"Wrote LCOV coverage to (.+)\.", report.strip())
    assert written, f"unexpected dump_lcov report: {report}"
    path = written.group(1)

    assert os.path.dirname(path) == echidna_server["corpus_dir"], \
        f"LCOV went somewhere other than the corpus directory: {path}"
    with open(path) as f:
        assert "SF:" in f.read(), "no source-file record in the LCOV output"


def test_reload_corpus_reads_the_corpus_directory(mcp_url):
    assert eventually(lambda: status(mcp_url)["corpus_size"] > 0), \
        "the campaign never wrote a corpus entry to reload"

    report = call_tool(mcp_url, "reload_corpus")

    # Every sequence on disk is one this campaign put there, so it has them all
    # already; what matters is that it read the directory and said what it found.
    assert re.match(r"Added \d+ of \d+ transaction sequences from ", report), \
        f"unexpected reload_corpus report: {report}"


def test_inject_fuzz_transactions_keeps_the_campaign_running(mcp_url):
    before = status(mcp_url)["iterations"]

    report = call_tool(mcp_url, "inject_fuzz_transactions", {
        "transactions": "mintTokens(0x1111111111111111111111111111111111111111, 100);"
                        "transferTokens(?, ?)"
    })
    assert re.fullmatch(r"Fuzzing that sequence on [1-9]\d* workers\.", report), \
        f"unexpected injection report: {report}"

    # A worker that choked on the injected sequence would stop reporting here.
    assert eventually(lambda: status(mcp_url)["iterations"] > before), \
        "the campaign stopped fuzzing after an injection"


def test_inject_fuzz_transactions_checks_the_sequence(mcp_url):
    with pytest.raises(ToolError, match="No function 'nosuchfunction'"):
        call_tool(mcp_url, "inject_fuzz_transactions",
                  {"transactions": "nosuchfunction(1)"})

    with pytest.raises(ToolError, match="takes 2 arguments, not 1"):
        call_tool(mcp_url, "inject_fuzz_transactions",
                  {"transactions": "transferTokens(?)"})

    with pytest.raises(ToolError, match="Could not parse"):
        call_tool(mcp_url, "inject_fuzz_transactions",
                  {"transactions": "not a call sequence"})


def test_clear_fuzz_priorities_returns_the_workers_to_the_corpus(mcp_url):
    call_tool(mcp_url, "inject_fuzz_transactions",
              {"transactions": "mintTokens(?, ?)"})

    report = call_tool(mcp_url, "clear_fuzz_priorities")
    assert re.fullmatch(r"Cleared injected sequences on [1-9]\d* workers\.", report), \
        f"unexpected clear report: {report}"

    assert eventually(lambda: status(mcp_url)["iterations"] > 0), \
        "the campaign stopped fuzzing after clearing its priorities"


def test_execute_sequence_reports_every_call(mcp_url):
    report = json.loads(call_tool(mcp_url, "execute_sequence", {
        "transactions": "mintTokens(0x10, 1000); transferTokens(0x20, 500)"
    }))

    assert report["status"] == "completed"
    assert report["failed_tx_index"] is None
    assert report["transaction_count"] == 2

    minted, transferred = report["transactions"]
    assert minted["call"].endswith("mintTokens(0x10,1000)")
    assert transferred["call"].endswith("transferTokens(0x20,500)")
    for tx in (minted, transferred):
        assert tx["status"] == "completed", f"{tx['call']} did not complete: {tx}"
        assert tx["gas_used"] > 0
        assert any("Transfer" in log for log in tx["logs"]), \
            f"{tx['call']} emitted no Transfer: {tx['logs']}"


def test_execute_sequence_reports_a_revert(mcp_url):
    # More than the whole supply, so SimpleToken.transfer's require fails.
    report = json.loads(call_tool(mcp_url, "execute_sequence", {
        "transactions": "transferTokens(0x20, 999999999999999999999999999999)"
    }))

    assert report["status"] == "reverted"
    assert report["failed_tx_index"] == 1
    reverted, = report["transactions"]
    assert reverted["status"] == "reverted"
    assert any("Insufficient balance" in log for log in reverted["logs"]), \
        f"the revert reason is not in the report: {reverted['logs']}"


def test_execute_sequence_can_return_a_trace(mcp_url):
    report = json.loads(call_tool(mcp_url, "execute_sequence", {
        "transactions": "mintTokens(0x10, 1)", "trace": True
    }))

    assert "SimpleToken::mint" in report["trace"], \
        f"the inner call is not in the trace: {report.get('trace')}"


def test_execute_sequence_needs_concrete_arguments(mcp_url):
    with pytest.raises(ToolError, match="has to be concrete"):
        call_tool(mcp_url, "execute_sequence",
                  {"transactions": "transferTokens(?, 1)"})


def test_sample_records_what_a_function_does(mcp_url):
    report = call_tool(mcp_url, "sample", {"function": "transferTokens"})
    assert re.fullmatch(
        r"Sampling transferTokens\(address,uint256\) on [1-9]\d* workers\.", report
    ), f"unexpected sample report: {report}"

    sampled = eventually(lambda: status(mcp_url)["samples"])
    assert sampled, "the campaign never reported a sample"
    stats, = sampled
    assert stats["function"] == "transferTokens(address,uint256)"
    assert stats["calls"] > 0
    assert 0 <= stats["reverts"] <= stats["calls"]
    # A random uint256 is more than the supply, so most calls revert with a
    # reason, and the tail of them is what an agent reads.
    def recent_reverts():
        sampled = status(mcp_url)["samples"]
        return sampled and sampled[0]["recent_reverts"]

    assert eventually(recent_reverts), \
        "a function that reverts reported no revert summaries"

    assert call_tool(mcp_url, "sample", {"function": "off"}).startswith("Stopped sampling")
    assert eventually(lambda: status(mcp_url)["samples"] == []), \
        "the campaign kept sampling after being told to stop"


def test_sample_needs_a_function_that_exists(mcp_url):
    with pytest.raises(ToolError, match="No function 'nosuchfunction'"):
        call_tool(mcp_url, "sample", {"function": "nosuchfunction"})

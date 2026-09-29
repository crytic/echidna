# Echidna MCP agent example

An agent that connects to a live Echidna campaign through its [MCP](https://modelcontextprotocol.io/)
server, watches what the fuzzer is reaching, and aims it at what it is not.

## Requirements

```
pip install langchain-anthropic langgraph httpx
```

## Usage

**1. Start Echidna with the MCP server:**

```bash
echidna MyContract.sol --server 8080 --format text
```

`--format text` is required — the interactive TUI otherwise owns the terminal.

**2. Run the agent:**

```bash
export ANTHROPIC_API_KEY=your_key_here
python examples/mcp_agent.py
```

It polls until you stop it with Ctrl-C. Point it at another campaign with
`ECHIDNA_MCP_URL=http://127.0.0.1:9000/mcp`.

## What it does

Every 30 seconds it calls `status`. When the campaign has gone a minute without
finding new coverage, it reads `target` and `show_coverage`, asks Claude for
call sequences that would reach lines the campaign has not, and injects them
with `inject_fuzz_transactions` — clearing the previous round first, so the
fuzzer is not left splitting its budget across every ordering ever suggested.

## The tools the server exposes

Four report on the campaign:

| Tool | Description |
|------|-------------|
| `status` | Corpus size, iterations, coverage, failing tests, optimization values, how long since the last coverage and which functions found it, and whatever `sample` is recording |
| `target` | The contract under test and the functions it exposes |
| `show_coverage` | One contract's source, line by line, marked with what the campaign reached |
| `dump_lcov` | Write the coverage so far to an LCOV file |

Five steer it:

| Tool | Description |
|------|-------------|
| `inject_fuzz_transactions` | Spend part of the fuzzer's budget on a specific ordering of calls |
| `clear_fuzz_priorities` | Forget every injected ordering and return to the corpus |
| `execute_sequence` | Run a concrete sequence and report what each call did, without disturbing the campaign |
| `sample` | Record what one function does as the campaign calls it; results appear in `status` |
| `reload_corpus` | Pick up whatever was written to the corpus directory since the campaign started |

`inject_fuzz_transactions` and `execute_sequence` take a sequence written the way
the calls would be in Solidity, separated by `;`. `inject_fuzz_transactions`
additionally accepts `?` for an argument the fuzzer should choose:

```
approve(0x10, ?); transferFrom(?, ?, 100)
```

## Testing the server

`tests/mcp/` drives a real campaign the way these clients do — tool semantics,
wire-protocol conformance, and the official MCP SDK. See the
[README](../README.md#running-the-test-suites) for how to run it.

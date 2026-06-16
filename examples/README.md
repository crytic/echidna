# Echidna MCP Agent Example

An AI agent that connects to a live Echidna fuzzing campaign via the [MCP](https://spec.modelcontextprotocol.io/) server and autonomously guides the fuzzer.

## Requirements

```
pip install langchain-anthropic langgraph httpx
```

## Usage

**1. Start Echidna with the MCP server enabled:**

```bash
echidna MyContract.sol --server 8080 --format text
```

The `--format text` flag is required — it disables the interactive TUI so the MCP server thread can run.

**2. Run the agent:**

```bash
export ANTHROPIC_API_KEY=your_key_here
python examples/mcp_agent.py
```

## What the agent does

The agent runs in a loop at a configurable interval. Each step it:

1. **Observes** — calls `status` to read coverage, iterations, and corpus size.
2. **Injects** — when coverage stagnates, asks Claude to suggest targeted transaction sequences and injects them via `inject_fuzz_transactions`.
3. **Resets** — periodically calls `clear_fuzz_priorities` to prevent the fuzzer from getting stuck on a single function.

## Available MCP tools

| Tool | Description |
|------|-------------|
| `status` | Campaign metrics: coverage, iterations, corpus size, last log lines |
| `target` | ABI of the target contract |
| `show_coverage` | Per-contract source coverage with line annotations |
| `dump_lcov` | Export coverage in LCOV format |
| `inject_fuzz_transactions` | Inject a semicolon-separated sequence of function calls |
| `clear_fuzz_priorities` | Reset function call weighting on all workers |
| `reload_corpus` | Reload transaction sequences from the corpus directory |

Transactions passed to `inject_fuzz_transactions` use Solidity call syntax and support `?` as a fuzzer wildcard:

```
transfer(0xdeadbeef00000000000000000000000000000001, 100);approve(?, ?)
```

# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## About Echidna

Echidna is a Haskell-based smart contract fuzzer designed for property-based testing of Ethereum smart contracts. It uses sophisticated grammar-based fuzzing campaigns to find bugs and verify invariants in Solidity code.

## Development Commands

### Building
```bash
# Enter the Nix development shell first
nix develop

# Build the project using Cabal
cabal build

# Build with profiling enabled (for performance debugging)
cabal --enable-profiling build
```

### Running Tests
```bash
# Run the full test suite
cabal run tests

# Run tests in a specific directory context
cabal test
```

### Running Echidna
```bash
# Run Echidna on a contract
cabal run echidna -- contract.sol

# Run with config file
cabal run echidna -- contract.sol --config config.yaml

# Run with profiling for performance analysis
cabal --enable-profiling run echidna -- contract.sol +RTS -p -s
```

### Development Environment
- Uses Nix for dependency management and development environment
- Primary build system is Cabal (not Stack, despite stack.yaml presence)
- **Important:** Always work inside `nix develop` shell for consistent environment

## Architecture Overview

### Core Components
- **lib/Echidna.hs**: Main entry point for contract preparation and setup
- **lib/Echidna/Campaign.hs**: Fuzzing campaign logic and execution
- **lib/Echidna/Types/**: Core type definitions (Config, Test, Tx, World, etc.)
- **lib/Echidna/Solidity.hs**: Solidity compilation and contract handling
- **lib/Echidna/ABI.hs**: ABI encoding/decoding for transaction generation

### Key Workflows
1. **Contract Preparation** (`prepareContract`): Compiles contracts, extracts tests, creates VM
2. **Test Generation**: Uses Slither for static analysis to guide fuzzing
3. **Campaign Execution**: Generates random transaction sequences to test invariants
4. **Coverage Analysis**: Tracks code coverage to guide fuzzing toward unexplored paths

### Testing Framework
- Uses Tasty for test organization
- Tests are located in `src/test/Tests/`
- Integration tests in `tests/solidity/` cover various Solidity patterns
- Test runner automatically changes to `./tests/solidity` directory

### Configuration System
- YAML-based configuration files (see `tests/solidity/basic/default.yaml`)
- Main config types in `lib/Echidna/Types/Config.hs`
- Supports campaign, UI, transaction, and Solidity-specific settings

## Important Development Notes

- The project uses GHC2021 language standard with several extensions enabled
- Profiling builds available for performance analysis of fuzzing campaigns
- Static analysis integration with Slither for enhanced test generation
- Supports multiple output formats (text, JSON, interactive UI)
# Echidna repository guidance

Echidna is a Haskell fuzzer for Ethereum smart contracts. It generates ABI-based
transaction sequences to check Solidity properties, assertions, and invariants.

## Where to look

- `lib/Echidna.hs` prepares contracts and their initial VM state.
- `lib/Echidna/Worker/`, `lib/Echidna/Mutator/`, and `lib/Echidna/Types/`
  contain campaign execution, mutations, and shared state.
- `lib/Echidna/Solidity.hs`, `lib/Echidna/ABI.hs`, and
  `lib/Echidna/SourceAnalysis/Slither.hs` handle contracts, ABI data, and
  Slither analysis.
- `src/Main.hs` defines CLI options, `lib/Echidna/Exec.hs` executes EVM
  transactions, and `lib/Echidna/MCP.hs` implements the live campaign tools.
- `lib/Echidna/Types/Config.hs` defines configuration types; see
  `tests/solidity/basic/default.yaml` for an example.
- `src/test/Spec.hs` registers the Haskell Tasty suite, `src/test/Common.hs`
  provides contract test helpers, `tests/solidity/` holds contract fixtures,
  and `tests/mcp/` contains Python MCP integration tests.

## Build and verification

- Use `nix develop` for the local toolchain (`cabal`, `solc`, Slither, and
  `hlint`). The shell runs `hpack`; edit `package.yaml` for package changes,
  since `echidna.cabal` is generated and ignored.
- From the repository root, run `cabal build` and
  `cabal test echidna-testsuite` for Haskell changes. The test runner changes
  into `tests/solidity`, so invoke it from the root. Some integration cases
  skip for incompatible `solc` versions; CI covers multiple versions. Test
  compiler-sensitive changes with the relevant version.
- For MCP server or client changes, put a built `echidna` on `PATH` and run
  `pytest tests/mcp -v` after installing `tests/mcp/requirements-test.txt`.
  Ensure `crytic-compile` and a Solidity 0.8.x compiler are available: the
  MCP fixture requires `^0.8.0`, while the Nix shell may select 0.5.7.
  CI uses `solc` 0.8.25. The README documents `ECHIDNA_BIN`,
  `ECHIDNA_MCP_URL`, and `ECHIDNA_MCP_PORT`.
- Run `cabal run echidna -- tests/solidity/basic/flags.sol` for a quick manual
  fuzzing check when relevant. Use `cabal --enable-profiling build` for profiling
  work.
- Main CI builds with Stack. Check `.github/workflows/` when changing build or
  test setup.

## Code conventions

- Follow the GHC2021 language setting and extensions in `package.yaml`.
- In Haskell modules, separate external, `EVM.*`, and `Echidna.*` imports with
  blank lines. Keep imports ordered within each group when editing them.
- Keep changes focused; avoid unrelated formatting changes, as requested in
  `CONTRIBUTING.md`.

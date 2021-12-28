# Echidna: A Fast Smart Contract Fuzzer <a href="https://raw.githubusercontent.com/crytic/echidna/master/echidna.png"><img src="https://raw.githubusercontent.com/crytic/echidna/master/echidna.png" width="75"/></a>

![Build Status](https://github.com/crytic/echidna/workflows/CI/badge.svg)

Echidna is a weird creature that eats bugs and is highly electrosensitive (with apologies to Jacob Stanley)

More seriously, Echidna is a Haskell program designed for fuzzing/property-based testing of Ethereum smarts contracts. It uses sophisticated grammar-based fuzzing campaigns based on a [contract ABI](https://solidity.readthedocs.io/en/develop/abi-spec.html) to falsify user-defined predicates or [Solidity assertions](https://solidity.readthedocs.io/en/develop/control-structures.html#id4). We designed Echidna with modularity in mind, so it can be easily extended to include new mutations or test specific contracts in specific cases.

## Features

* Generates inputs tailored to your actual code
* Optional corpus collection, mutation and coverage guidance to find deeper bugs
* Powered by [Slither](https://github.com/crytic/slither) to extract useful information before the fuzzing campaign
* Source code integration to identify which lines are covered after the fuzzing campaign
* Curses-based retro UI, text-only or JSON output
* Automatic testcase minimization for quick triage
* Seamless integration into the development workflow
* Maximum gas usage reporting of the fuzzing campaign
* Support for a complex contract initialization with [Etheno](https://github.com/crytic/etheno) and Truffle

.. and [a beautiful high-resolution handcrafted logo](https://raw.githubusercontent.com/crytic/echidna/master/echidna.png).

## Screenshot

<a href="https://trailofbits.files.wordpress.com/2020/03/image5.png"><img src="https://trailofbits.files.wordpress.com/2020/03/image5.png" width="650"/></a>

## Usage

### Executing the test runner

The core Echidna functionality is an executable called `echidna-test`. `echidna-test` takes a contract and a list of invariants (properties that should always remain true) as input. For each invariant, it generates random sequences of calls to the contract and checks if the invariant holds. If it can find some way to falsify the invariant, it prints the call sequence that does so. If it can't, you have some assurance the contract is safe.

### Writing invariants

Invariants are expressed as Solidity functions with names that begin with `echidna_`, have no arguments, and return a boolean. For example, if you have some `balance` variable that should never go below `20`, you can write an extra function in your contract like this one:

```solidity
function echidna_check_balance() public returns (bool) {
    return(balance >= 20);
}
```

To check these invariants, run:

```
$ echidna-test myContract.sol
```

An example contract with tests can be found [examples/solidity/basic/flags.sol](examples/solidity/basic/flags.sol). To run it, you should execute:
```
$ echidna-test examples/solidity/basic/flags.sol
```

Echidna should find a a call sequence that falsifies `echidna_sometimesfalse` and should be unable to find a falsifying input for `echidna_alwaystrue`.

### Collecting and visualizing coverage

After finishing a campaign, Echidna can save a coverage maximizing **corpus** in a special directory specified with the `corpusDir` config option. This directory will contain two entries: (1) a directory named `coverage` with JSON files that can be replayed by Echidna and (2) a plain-text file named `covered.txt`, a copy of the source code with coverage annotations.

If you run `examples/solidity/basic/flags.sol` example, Echidna will save a few files serialized transactions in the `coverage` directory and a `covered.$(date +%s).txt` file with the following lines:

```
*r  |  function set0(int val) public returns (bool){
*   |    if (val % 100 == 0)
*   |      flag0 = false;
  }

*r  |  function set1(int val) public returns (bool){
*   |    if (val % 10 == 0 && !flag0)
*   |      flag1 = false;
  }
```

Our tool signals each execution trace in the corpus with the following "line marker":
 - `*` if an execution ended with a STOP
 - `r` if an execution ended with a REVERT
 - `o` if an execution ended with an out-of-gas error
 - `e` if an execution ended with any other error (zero division, assertion failure, etc) 

### Crash course on Echidna

Our [Building Secure Smart Contracts](https://github.com/crytic/building-secure-contracts) repository contains a crash course on Echidna, including examples, lessons and exercises. You should [start here](https://github.com/crytic/building-secure-contracts/tree/master/program-analysis/echidna#echidna-tutorial).

### Support for smart contract build systems

Echidna can test contracts compiled with different smart contract build systems, including [Truffle](https://truffleframework.com/), [Embark](https://framework.embarklabs.io/) and even [Vyper](https://vyper.readthedocs.io), using [crytic-compile](https://github.com/crytic/crytic-compile). For instance,
we can uncover an integer overflow in the [Metacoin Truffle box](https://github.com/truffle-box/metacoin-box) using a
[contract with Echidna properties to test](examples/solidity/truffle/metacoin/contracts/MetaCoinEchidna.sol):

```
$ cd examples/solidity/truffle/metacoin
$ echidna-test . --contract TEST
...
echidna_convert: failed!💥
  Call sequence:
    mint(57896044618658097711785492504343953926634992332820282019728792003956564819968)
```

Echidna supports two modes of testing complex contracts. Firstly, one can [describe an initialization procedure with Truffle and Etheno](https://github.com/crytic/echidna/wiki/Deployment-of-a-contract-using-Truffle,-Ganache-and-Etheno-to-test-with-Echidna) and use that as the base state for Echidna. Secondly, echidna can call into any contract with a known ABI by passing in the corresponding solidity source in the CLI. Use `multi-abi: true` in your config to turn this on.

### Configuration options

Echidna's CLI can be used to choose the contract to test and load a
configuration file.

```
$ echidna-test contract.sol --contract TEST --config config.yaml
```

The configuration file allows users to choose EVM and test generation
parameters. An example of a complete and annotated config file with the default
options can be found at
[examples/solidity/basic/default.yaml](examples/solidity/basic/default.yaml).
More detailed documentation on the configuration options is available in our
[wiki](https://github.com/trailofbits/echidna/wiki/Config).

Echidna supports three different output drivers. There is the default `text`
driver, a `json` driver, and a `none` driver, which should suppress all
`stdout` output. The JSON driver reports the overall campaign as follows.


```json
Campaign = {
  "success"      : bool,
  "error"        : string?,
  "tests"        : [Test],
  "seed"         : number,
  "coverage"     : Coverage,
  "gas_info"     : [GasInfo]
}
Test = {
  "contract"     : string,
  "name"         : string,
  "status"       : string,
  "error"        : string?,
  "testType"     : string,
  "transactions" : [Transaction]?
}
Transaction = {
  "contract"     : string,
  "function"     : string,
  "arguments"    : [string]?,
  "gas"          : number,
  "gasprice"     : number
}
```

`Coverage` is a dict describing certain coverage increasing calls.
Each `GasInfo` entry is a tuple that describes how maximal
gas usage was achieved, and also not too important. These interfaces are
subject to change to be slightly more user friendly at a later date. `testType`
will either be `property` or `assertion`, and `status` always takes on either
`fuzzing`, `shrinking`, `solved`, `passed`, or `error`.

## Limitations and known issues

EVM emulation and testing is hard. Echidna has a number of limitations in the latest release. Some of these are inherited from [hevm](https://github.com/dapphub/dapptools/tree/master/src/hevm) while some are results from design/performance decisions or simply bugs in our code. We list them here including their corresponding issue and the status ("wont fix", "in review", "fixed"). Issues that are "fixed" are expected to be included in the next Echidna release.

| Description |  Issue   | Status   |  
| :--- |     :---:              |         :---:   |
| Debug information can be insufficient | [#656](https://github.com/crytic/echidna/issues/656) | *[in review for 2.0](https://github.com/crytic/echidna/pull/674)* |
| Vyper support is limited | [#652](https://github.com/crytic/echidna/issues/652) | *wont fix* |
| Limited library support for testing | [#651](https://github.com/crytic/echidna/issues/651) | *wont fix* |
| If the contract is not properly linked, Echidna will crash | [#514](https://github.com/crytic/echidna/issues/514) | *in review* | 
| Assertions are not detected in internal transactions | [#601](https://github.com/crytic/echidna/issues/601) | *[in review for 2.0](https://github.com/crytic/echidna/pull/674)* |
| Assertions are not detected in solc 0.8.x | [#669](https://github.com/crytic/echidna/issues/669) | *[in review for 2.0](https://github.com/crytic/echidna/pull/674)* |
| Value generation can fail in multi-abi mode, since the function hash is not precise enough | [#579](https://github.com/crytic/echidna/issues/579) | *[in review for 2.0](https://github.com/crytic/echidna/pull/674)*|

## Installation

### Precompiled binaries

Before starting, make sure Slither is [installed](https://github.com/crytic/slither) (`pip3 install slither-analyzer --user`).
If you want to quickly test Echidna in Linux or MacOS, we provide statically linked Linux binaries built on Ubuntu and mostly static MacOS binaries on our [releases page](https://github.com/crytic/echidna/releases). You can also grab the same type of binaries from our [CI pipeline](https://github.com/crytic/echidna/actions?query=workflow%3ACI+branch%3Amaster+event%3Apush), just click the commit to find binaries for Linux or MacOS.

### Docker container

If you prefer to use a pre-built Docker container, check out our [docker packages](https://github.com/orgs/crytic/packages?repo_name=echidna), which are auto-built via Github Actions. Two container variants are available:

* **debian**: Based on `debian:bullseye-slim`, it is meant to be a small yet flexible enough image to use echidna on. It provides a pre-built version of `echidna-test`, as well as `slither`, `crytic-compile`, `solc-select` and `nvm` under 200 MB.
* **distroless**: Based on `distroless/python3-debian11:nonroot`, it is meant to be a very small image with the bare minimum required to run echidna. It provides a pre-built version of `echidna-test`, as well as `slither`, `crytic-compile` and `solc-select`. No shell or `nvm` is provided in this image. A [wrapper](docker/solc-install.py) to install a `solc` version before running echidna is included, which will observe the `SOLC_VERSION` environment variable.

Different tags are available on each container:

| Tag           | Build in tag
|---------------|-------------
| `vx.y.z`      | Build corresponding to release `vx.y.z`
| `latest`      | Latest Echidna tagged release.
| `edge`        | Most recent commit on the default branch.
| `testing-foo` | Testing build based on the `foo` branch.

To run the Debian variant container with the latest Echidna version interactively, you can use something like the following command. It will map the current directory as `/src` inside the container, and give you a shell where you can use `echidna-test`:

```sh
$ docker run --rm -it -v `pwd`:/src ghcr.io/crytic/echidna/debian
```

Otherwise, if you want to locally build the latest version of Echidna, we recommend using Docker. From within a clone of this repository, run the following command to build the debian variant of the Docker container image:

```sh
$ docker build -t echidna -f docker/Dockerfile --target final-debian .
```

Then, you can run the `echidna` image locally. For example, to install solc 0.5.7 and check `examples/solidity/basic/flags.sol`, you can run:

```sh
$ docker run -it -v `pwd`:/src echidna bash -c "solc-select install 0.5.7 && solc-select use 0.5.7 && echidna-test /src/examples/solidity/basic/flags.sol"
```

### Building using Stack

If you'd prefer to build from source, use [Stack](https://docs.haskellstack.org/en/stable/README/). `stack install` should build and compile `echidna-test` in `~/.local/bin`. You will need to link against libreadline and libsecp256k1 (built with recovery enabled), which should be installed with the package manager of your choosing. You also need to install the latest release of [libff](https://github.com/scipr-lab/libff). Refer to our [CI tests](.github/scripts/install-libff.sh) for guidance.

Some Linux distributions do not ship static libraries for certain things that Haskell needs, e.g. Arch Linux, which will cause `stack build` to fail with linking errors because we use the `-static` flag. Removing these from `package.yaml` should get everything to build if you are not looking for a static build.

If you're getting errors building related to linking, try tinkering with `--extra-include-dirs` and `--extra-lib-dirs`.

### Building using Nix

Nix users can install the lastest Echidna with:
```
$ nix-env -i -f https://github.com/crytic/echidna/tarball/master
```

It is possible to develop Echidna with Cabal inside `nix-shell`. Nix will automatically
install all the dependencies required for development including `crytic-compile` and `solc`.
A quick way to get GHCi with Echidna ready for work:
```
$ git clone https://github.com/crytic/echidna
$ cd echidna
$ nix-shell
[nix-shell]$ cabal new-repl
```

Running the test suite:
```
nix-shell --run 'cabal test'
```

## Getting help

Feel free to stop by our #ethereum slack channel in [Empire Hacking](https://empireslacking.herokuapp.com/) for help using or extending Echidna.

* Get started by reviewing these simple [Echidna invariants](examples/solidity/basic/flags.sol)

* Review the [Solidity examples](examples/solidity) directory for more extensive Echidna use cases

* Considering [emailing](mailto:echidna-dev@trailofbits.com) the Echidna development team directly for more detailed questions

## License

Echidna is licensed and distributed under the [AGPLv3 license](https://github.com/crytic/echidna/blob/master/LICENSE).

## Projects using Echidna

This is a partial list of smart contracts projects that use Echidna for testing:

* [Uniswap-v3](https://github.com/search?q=org%3AUniswap+echidna&type=commits)
* [Balancer](https://github.com/balancer-labs/balancer-core/tree/master/echidna)
* [MakerDAO vest](https://github.com/makerdao/dss-vest/pull/16)
* [Optimism DAI Bridge](https://github.com/BellwoodStudios/optimism-dai-bridge/blob/master/contracts/test/DaiEchidnaTest.sol)
* [WETH10](https://github.com/WETH10/WETH10/tree/main/contracts/fuzzing)
* [Yield](https://github.com/yieldprotocol/fyDai/pull/312)
* [Convexity Protocol](https://github.com/opynfinance/ConvexityProtocol/tree/dev/contracts/echidna)
* [Aragon Staking](https://github.com/aragon/staking/blob/82bf54a3e11ec4e50d470d66048a2dd3154f940b/packages/protocol/contracts/test/lib/EchidnaStaking.sol)
* [Centre Token](https://github.com/centrehq/centre-tokens/tree/master/echidna_tests)
* [Tokencard](https://github.com/tokencard/contracts/tree/master/tools/echidna)
* [Minimalist USD Stablecoin](https://github.com/usmfum/USM/pull/41) 

## Trophies

### Security Issues

The following lists security vulnerabilities that were found by Echidna. If you found a security vulnerability using our tool, please submit a PR with the relevant information.

| Project | Vulnerability | Date |
|--|--|--|
[0x Protocol](https://github.com/trailofbits/publications/blob/master/reviews/0x-protocol.pdf) | If an order cannot be filled, then it cannot be canceled | Oct 2019
[0x Protocol](https://github.com/trailofbits/publications/blob/master/reviews/0x-protocol.pdf) | If an order can be partially filled with zero, then it can be partially filled with one token | Oct 2019
[0x Protocol](https://github.com/trailofbits/publications/blob/master/reviews/0x-protocol.pdf) | The cobbdouglas function does not revert when valid input parameters are used | Oct 2019
[Balancer Core](https://github.com/trailofbits/publications/blob/master/reviews/BalancerCore.pdf) | An attacker cannot steal assets from a public pool | Jan 2020
[Balancer Core](https://github.com/trailofbits/publications/blob/master/reviews/BalancerCore.pdf) | An attacker cannot generate free pool tokens with joinPool | Jan 2020
[Balancer Core](https://github.com/trailofbits/publications/blob/master/reviews/BalancerCore.pdf) | Calling joinPool-exitPool does not lead to free pool tokens | Jan 2020
[Balancer Core](https://github.com/trailofbits/publications/blob/master/reviews/BalancerCore.pdf) |  Calling exitswapExternAmountOut does not lead to free assets | Jan 2020
[Yield Protocol](https://github.com/trailofbits/publications/blob/master/reviews/YieldProtocol.pdf) | Arithmetic computation for buying and selling tokens is imprecise | Aug 2020
[Origin Dollar](https://github.com/trailofbits/publications/blob/master/reviews/OriginDollar.pdf) | Users are allowed to transfer more tokens that they have | Nov 2020
[Origin Dollar](https://github.com/trailofbits/publications/blob/master/reviews/OriginDollar.pdf) | User balances can be larger than total supply | Nov 2020
[Liquity Dollar](https://github.com/trailofbits/publications/blob/master/reviews/Liquity.pdf) | Closing troves require to hold the full amount of LUSD minted | Dec 2020
[Liquity Dollar](https://github.com/trailofbits/publications/blob/master/reviews/Liquity.pdf) | Troves can be improperly removed | Dec 2020
[Liquity Dollar](https://github.com/trailofbits/publications/blob/master/reviews/Liquity.pdf) | Initial redeem can revert unexpectedly | Dec 2020
[Liquity Dollar](https://github.com/trailofbits/publications/blob/master/reviews/Liquity.pdf) | Redeem without redemptions might still return success | Dec 2020

### Research Examples

We can also use Echidna to reproduce a number of research examples from smart contract fuzzing papers to show how quickly it can find the solution:

| Source | Code 
|--|--
[Using automatic analysis tools with MakerDAO contracts](https://forum.openzeppelin.com/t/using-automatic-analysis-tools-with-makerdao-contracts/1021) | [SimpleDSChief](https://github.com/crytic/echidna/blob/master/examples/solidity/research/vera_dschief.sol)
[Integer precision bug in Sigma Prime](https://github.com/b-mueller/sabre#example-2-integer-precision-bug) | [VerifyFunWithNumbers](https://github.com/crytic/echidna/blob/master/examples/solidity/research/solcfuzz_funwithnumbers.sol)
[Learning to Fuzz from Symbolic Execution with Application to Smart Contracts](https://files.sri.inf.ethz.ch/website/papers/ccs19-ilf.pdf) | [Crowdsale](https://github.com/crytic/echidna/blob/master/examples/solidity/research/ilf_crowdsale.sol)
[Harvey: A Greybox Fuzzer for Smart Contracts](https://arxiv.org/abs/1905.06944) | [Foo](https://github.com/crytic/echidna/blob/master/examples/solidity/research/harvey_foo.sol), [Baz](https://github.com/crytic/echidna/blob/master/examples/solidity/research/harvey_baz.sol)

All these can be solved, from a few seconds to one or two minutes on a laptop computer.

## Publications

### Trail of Bits
- [Echidna: effective, usable, and fast fuzzing for smart contracts](https://github.com/trailofbits/publications/blob/master/papers/echidna_issta2020.pdf), Gustavo Grieco, Will Song, Artur Cygan, Josselin  Feist, Alex Groce - ISSTA '20
- [echidna-parade: A Tool for Diverse Multicore Smart Contract Fuzzing](https://agroce.github.io/issta21.pdf), Alex Groce, Gustavo Grieco - ISSTA '21

If you are using Echidna on an academic work, consider applying to the [Crytic $10k Research Prize](https://blog.trailofbits.com/2019/11/13/announcing-the-crytic-10k-research-prize/).

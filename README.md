# Echidna: A Fast Smart Contract Fuzzer <a href="https://raw.githubusercontent.com/crytic/echidna/master/echidna.png"><img src="https://raw.githubusercontent.com/crytic/echidna/master/echidna.png" width="75"/></a>

![Build Status](https://github.com/crytic/echidna/workflows/CI/badge.svg)

Echidna is a weird creature that eats bugs and is highly electrosensitive (with apologies to Jacob Stanley)

More seriously, Echidna is a Haskell program designed for fuzzing/property-based testing of Ethereum smarts contracts. It uses sophisticated grammar-based fuzzing campaigns based on a [contract ABI](https://solidity.readthedocs.io/en/develop/abi-spec.html) to falsify user-defined predicates or [Solidity assertions](https://solidity.readthedocs.io/en/develop/control-structures.html#id4). We designed Echidna with modularity in mind, so it can be easily extended to include new mutations or test specific contracts in specific cases.

## Features

* Generates inputs tailored to your actual code
* Optional corpus collection, mutation and coverage guidance to find deeper bugs
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

Echidna should find a a call sequence that falisfies `echidna_sometimesfalse` and should be unable to find a falsifying input for `echidna_alwaystrue`.

### Crash course on Echidna

Our [Builiding Secure Smart Contracts](https://github.com/crytic/building-secure-contracts) repository contains a crash course on Echidna, including examples, lessons and exercises. You should [start here](https://github.com/crytic/building-secure-contracts/tree/master/program-analysis/echidna#echidna-tutorial).

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

Echidna's CLI can be used to choose the contract to test and load a configuration file.

```
$ echidna-test contract.sol --contract TEST --config config.yaml
```

The configuration file allows users to choose EVM and test generation
parameters. An example of a complete and annotated config file with the default options can be found at [examples/solidity/basic/default.yaml](examples/solidity/basic/default.yaml). More detailed documentation on the configuration options is available in our [wiki](https://github.com/trailofbits/echidna/wiki/Config).

## Installation

If you want to quickly test Echidna in Linux or MacOS, we provide statically linked Linux binaries built on Ubuntu and mostly static MacOS binaries on our [releases page](https://github.com/crytic/echidna/releases). You can also grab the same type of binaries from our [CI pipeline](https://github.com/crytic/echidna/actions?query=workflow%3ACI+branch%3Amaster+event%3Apush), just click the commit to find binaries for Linux or MacOS. If you are logged into Github on your local `docker` client, you can check out our [docker package](https://github.com/crytic/echidna/packages/136575), which are also auto-built via Github Actions.
If you want to install the latest released version of Echidna, we recommend using docker:

```
$ docker build -t echidna .
```

Then, run it via:

```
$ docker run -it -v `pwd`:/src echidna echidna-test /src/examples/solidity/basic/flags.sol
```

If you'd prefer to build from source, use [Stack](https://docs.haskellstack.org/en/stable/README/). `stack install` should build and compile `echidna-test` in `~/.local/bin`. You will need to link against libreadline and libsecp256k1 (built with recovery enabled), which should be installed with the package manager of your choosing. You also need to install the latest release of [libff](https://github.com/scipr-lab/libff). Refer to our [CI tests](.github/scripts/install-libff.sh) for guidance.

Some Linux distributions do not ship static libraries for certain things that Haskell needs, e.g. Arch Linux, which will cause `stack build` to fail with linking errors because we use the `-static` flag. Removing these from `package.yaml` should get everything to build if you are not looking for a static build.

If you're getting errors building related to linking, try tinkering with `--extra-include-dirs` and `--extra-lib-dirs`.

Nix users can install the lastest Echidna with:
```
$ nix-env -i -f https://github.com/crytic/echidna/tarball/master
```

## Development

It is possible to develop Echidna with Cabal inside `nix-shell`. Nix will automatically
install all the dependencies required for development including `crytic-compile` and `solc`.
A quick way to get GHCi with Echidna ready for work:
```
$ git clone https://github.com/crytic/echidna
$ cd echidna
$ nix-shell
[nix-shell]$ cabal new-repl
```

## Getting help

Feel free to stop by our #ethereum slack channel in [Empire Hacking](https://empireslacking.herokuapp.com/) for help using or extending Echidna.

* Get started by reviewing these simple [Echidna invariants](examples/solidity/basic/flags.sol)

* Review the [Solidity examples](examples/solidity) directory for more extensive Echidna use cases

* Considering [emailing](mailto:echidna-dev@trailofbits.com) the Echidna development team directly for more detailed questions

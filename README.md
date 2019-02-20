# echidna

[![Build Status](https://travis-ci.org/trailofbits/echidna.svg?branch=master)](https://travis-ci.org/trailofbits/echidna)

![echidna logo](echidna.png)

Echidna is a weird creature that eats bugs and is highly electrosensitive (with apologies to Jacob Stanley)

More seriously, Echidna is a Haskell library designed for fuzzing/property-based testing of EVM code. It supports relatively sophisticated grammar-based fuzzing campaigns to falsify a variety of predicates.

## Features

* Generates inputs tailored to your actual code
* Optional coverage guidance to find deeper bugs
* Automatic testcase minimization for quick triage
* Seamless integration into the development workflow
* Fast
* Powerful API for advanced usage
* Beautiful logo

## Usage

### Executing the test runner

The core Echidna functionality is an executable called `echidna-test`. `echidna-test` takes a contract and a list of invariants (properties that should always remain true) as input. For each invariant, it generates random sequences of calls to the contract and checks if the invariant holds. If it can find some way to falsify the invariant, it prints the call sequence that does so. If it can't, you have some assurance the contract is safe.

### Writing invariants

Invariants are expressed as Solidity functions with names that begin with `echidna_`, have no arguments, and return a boolean. For example, if I have some `balance` variable that should never go below 20, I can write `function echidna_balance() { return(balance >= 20); }`. To check these invariants, run `echidna-test myContract.sol`.

An example contract with tests can be found [examples/solidity/basic/flags.sol](examples/solidity/basic/flags.sol). To run it, execute: 
```
$ echidna-test examples/solidity/basic/flags.sol
```

Echidna should find a a call sequence that falisfies `echidna_sometimesfalse` and should be unable to find a falsifying input for `echidna_alwaystrue`.

### Configuration options

Echidna's CLI can be used to choose the contract to test, turn on coverage guided testing, or load a configuration file.

```
echidna-test contract.sol TEST --coverage --config="config.yaml"
```

The configuration file allows users to choose EVM and test generation parameters. An example of a complete config file with the default options can be found at [examples/solidity/basic/default.yaml](examples/solidity/basic/default.yaml). More detailed documentation on the configuration options is available in our [wiki](https://github.com/trailofbits/echidna/wiki/Config).

### Advanced usage

Echidna exports an API to build powerful fuzzing systems, and has a multitude of configuration options. Unfortunately, these parts of the codebase change quickly and are thus poorly documented. The [examples/api directory](examples/api) or [Trail of Bits blog](https://blog.trailofbits.com/2018/05/03/state-machine-testing-with-echidna/) are excellent references, or use the references below to get in touch with us directly.

## Installation

[docker](https://www.docker.com/) is recommended to install Echidna.

```
docker pull trailofbits/echidna
docker run trailofbits/echidna
```

for example

```
docker run -t -v `pwd`:/src trailofbits/echidna echidna-test /src/examples/solidity/basic/flags.sol
```


If you'd prefer to build from source, use [Stack](https://docs.haskellstack.org/en/stable/README/).
`stack install` should build and compile `echidna-test` in `~/.local/bin`.
You will need to link against libreadline and libsecp256k1 (built with recovery enabled), which should be installed with the package manager of your choosing.
If you're getting errors building related to linking, try tinkering with `--extra-include-dirs` and `--extra-lib-dirs`.

## Getting help

Feel free to stop by our [Slack channel](https://empirehacking.slack.com/messages/C7KKY517H/) for help using or extending Echidna.

* Get started by reviewing these simple [Echidna invariants](examples/solidity/basic/flags.sol)

* Review the [Solidity examples](examples/solidity) directory for more extensive Echidna use cases

* Considering [emailing](mailto:jp@trailofbits.com) the Echidna development team directly for more detailed questions

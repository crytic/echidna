# echidna

[![Build Status](https://travis-ci.org/trailofbits/echidna.svg?branch=master)](https://travis-ci.org/trailofbits/echidna)

![echidna logo](echidna.png)

Echidna is a weird creature that eats bugs and is highly electrosensitive (with apologies to Jacob Stanley)

More seriously, it's a Haskell library designed for fuzzing/property based testing of EVM code.
Currently it is quite alpha, and the API isn't guaranteed to be functional, let alone stable.
It supports relatively sophisticated grammar-based fuzzing campaigns to falsify a variety of predicates.

## Features

  * Grammar-based input generation

  * Optional coverage guidance

  * Automatic testcase minimization

  * Pretty terminal interface

  * Powerful API

  * Beautiful logo

## Usage

The core Echidna functionality is in an executable called `echidna-test`, which is available in the Docker container referenced above.
`echidna-test` takes as input a contract with some invariants (properties that should always remain true).
For each invariant, it generates random sequences of calls to the contract and checks if the invariant holds.
If it can find some way to falsify the invariant, it prints the call sequence that does so.
If it can't, you have some assurance the contract is safe.

Invariants are simple solidity functions with names beginning `echidna_`, no arguments, and a boolean return value.
For example, if I have some `balance` variable that should never go below 20, I can write `function echidna_balance() { return(balance >= 20); }`.
To check these invariants, I just run `echidna-test myContract.sol`.

An example contract with tests can be found [solidity/cli.sol](solidity/cli.sol). Running
`echidna-test solidity/cli.sol` should find a call sequence such that `echidna_sometimesfalse` fails, but be unable to do so for `echidna_alwaystrue`.

Echidna can be customized with a variety of command line arguments. Users can pass optional command line arguments to choose the contract to test, turn on coverage guided testing, and load a configuration file. For example:
```
echidna-test solidity/cli.sol solidity/cli.sol:Test --coverage --config="solidity/config.yaml"
```
The configuration file allows users to choose various EVM and test generation parameters within Echidna and is in yaml format. An example config file, along with documentation, can be found at [solidity/config.yaml](solidity/config.yaml).

## Advanced Usage

Echidna exports an API that can be used to build powerful fuzzing systems, and has a multitude of configuration options.
Unfortunately, these parts of the codebase change quickly and are thus poorly documented.
The [examples directory](examples) or [Trail of Bits blog](https://blog.trailofbits.com/2018/05/03/state-machine-testing-with-echidna/) are excellent places to reference though, or contact the Trail of Bits team using the resources below.

## Installation

[docker](https://www.docker.com/) is highly recommended to install echidna.

```
docker pull trailofbits/echidna
docker run trailofbits/echidna
```

If you'd prefer to build from source it's possible to do that with [Stack](https://docs.haskellstack.org/en/stable/README/)

## Where To Get Help

Join us in #ethereum on the [Empire Hacking Slack](https://empireslacking.herokuapp.com), or email [JP Smith](mailto:jp@trailofbits.com) (the lead author) directly.

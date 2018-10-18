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

An example contract with tests can be found [solidity/cli.sol](solidity/cli.sol). Run
`echidna-test solidity/cli.sol` to kickoff a test run. In this demonstration, Echidna should find a a call sequence that falisfies `echidna_sometimesfalse` and should be unable to find a falsifying input for for `echidna_alwaystrue`.

### Configuration options

Echidna's CLI can be used to choose the contract to test, turn on coverage guided testing, or load a configuration file.

```
echidna-test solidity/cli.sol solidity/cli.sol:Test --coverage --config="solidity/config.yaml"
```

The configuration file allows users to choose EVM and test generation parameters. An example config file, along with documentation, can be found at [solidity/config.yaml](solidity/config.yaml).

### Advanced usage

Echidna exports an API to build powerful fuzzing systems, and has a multitude of configuration options. Unfortunately, these parts of the codebase change quickly and are thus poorly documented. The [examples directory](examples) or [Trail of Bits blog](https://blog.trailofbits.com/2018/05/03/state-machine-testing-with-echidna/) are excellent references, or use the references below to get in touch with us directly.

## Installation

[docker](https://www.docker.com/) is recommended to install Echidna.

```
docker pull trailofbits/echidna
docker run trailofbits/echidna
```

for example

```
docker run -v `pwd`:/src trailofbits/echidna echidna-test /src/solidity/cli.sol
```


If you'd prefer to build from source, use [Stack](https://docs.haskellstack.org/en/stable/README/).

## Getting help

Feel free to stop by our [Slack channel](https://empirehacking.slack.com/messages/C7KKY517H/) for help using or extending Echidna.

* Get started by reviewing these simple [Echidna invariants](solidity/cli.sol)

* Review the [Solidity examples](solidity/examples) directory for more extensive Echidna use cases

* Considering [emailing](mailto:jp@trailofbits.com) the Echidna development team directly for more detailed questions

# echidna

[![Build Status](https://travis-ci.org/trailofbits/echidna.svg?branch=master)](https://travis-ci.org/trailofbits/echidna)

![echidna logo](echidna.png)

Echidna is a weird creature that eats bugs and is highly electrosensitive (with apologies to Jacob Stanley)

More seriously, it's a Haskell library designed for fuzzing/property based testing of EVM code.
Currently it is quite alpha, and the API isn't guaranteed to be functional, let alone stable.
It supports relatively sophisticated grammar-based fuzzing campaigns to falsify a variety of predicates.

## Installation


To properly compile and install echidna we use [nix](https://nixos.org/nix/) to take care of every dependency. To install, just execute:

```
git clone https://github.com/trailofbits/echidna/ --recursive
cd echidna
nix-env -i stack
stack setup
stack install
```

Notably, if you are using stack, `stack ghci` will set up a REPL with all functions in scope.
This can be quite useful for playing around with the library.

## Docker Installation

Set your Docker service to allow at least 4GB of RAM and do not forget to clone this repository with its submodules:

`git clone https://github.com/trailofbits/echidna/ --recursive`

Then, go into the `echidna` directory and build the Dockerfile with the following command:

`docker build -t echidna .`

This will take a long time and will consume ~ 4GB of RAM on a 2017 Macbook Pro.

When it's finished run the example contract with a simple

`docker run echidna`

## Usage (as an executable)

Echidna builds an executable, `echidna-test` that can be used from the command line to fuzz solidity code.
It expects unit tests in the form of functions with names starting with `echidna_` that take no arguments and return a `bool` indicating success or failure.
For each unit test it finds, it will execute a fuzzing campaign to try and find a set of calls such that executing that call sequence, then the test either returns `false` or results in a VM failure.

An example contract with tests can be found [solidity/cli.sol](solidity/cli.sol). Running
`echidna-test solidity/cli.sol` should find a call sequence such that `echidna_sometimesfalse` fails, but be unable to do so for `echidna_alwaystrue`.

Echidna can be customized with a variety of command line arguments. Users can pass optional command line arguments to choose the contract to test, turn on coverage guided testing, and load a configuration file. For example:
```
echidna-test solidity/cli.sol solidity/cli.sol:Test --coverage --config="solidity/config.yaml"
```
The configuration file allows users to choose various EVM and test generation parameters within Echidna and is in yaml format. An example config file, along with documentation, can be found at [solidity/config.yaml](solidity/config.yaml).

## Usage (as a library)

Echidna is actively being developed with relatively little regard for stability.
As a result of this, there is a lack of extensive documentation at the present time.
Nevertheless, we provide a short working example that should be relatively instructional:

```haskell
module Main where

import Hedgehog hiding            (checkParallel)
import Hedgehog.Internal.Property (GroupName(..), PropertyName(..))

import Echidna.Exec
import Echidna.Solidity

main :: IO ()
main = do (v,a,ts) <- loadSolidity "test.sol" Nothing Nothing
          let prop t = (PropertyName $ show t, ePropertySeq (`checkETest` t) a v 10)
          _ <- checkParallel . Group (GroupName "test.sol") $ map prop ts
          return ()
```

This example can be used to test this small solidity contract:

```solidity
pragma solidity ^0.4.16;

contract Test {
  uint private counter=1;
  uint private last_counter=counter;

  function inc(uint val){
    last_counter = counter;
    counter += val;
  }

  function skip() {
    return;
  }

  function echidna_check_counter() returns (bool) {
    if (last_counter > counter) {
      selfdestruct(0);
    }
    return true;
  }
}
```

Then, we can use echidna to find a counterexample:

```
━━━ test.sol ━━━
  ✗ "echidna_check_counter" failed after 14 tests and 132 shrinks.
  
      │ Call sequence: inc(111022932527598298683654135258804814522795708541881158271458983003743791605633);
      │                inc(4769156709717896739916849749883093330474276123759405767998601004169338034302);
  
  ✗ 1 failed.
```

### [Echidna.ABI](lib/Echidna/ABI.hs)

This module provides Hedgehog generators for most of the EVM ABI.
It can be used without any other module to provide random "ASTs" (e.g. a random dynamic array of static arrays of 16 248-bit unsigned ints) or calldata (EVM-encoded function calls with these arguments).

Whenever possible, it tries to copy the convention of hevm.

### [Echidna.Exec](lib/Echidna/Exec.hs)

This module provides functionality for executing fuzzing campaigns.
It's highly recommended to use `ePropertySeq` and `checkParallel` to do this.
[The standalone executable](src/Main.hs) is a pretty good example of recommended usage.
`fuzz` can also be used, but if the predicate doesn't need IO, it's not recommended.

It also provides some convenience functions for writing custom campaigns (`checkETest`, `cleanUp`, `eCommand`, `execCall`.)
The [state machine example](examples/state-machine/StateMachine.hs) is a pretty good example of how to use these.

### [Echidna.Solidity](lib/Echidna/Solidity.hs)

This module provides `loadSolidity`, which takes a solidity source file and provides a VM with the first contract therein loaded as well as a `fuzz`-compatible ABI definition.

## Questions/complaints/etc.

Join us in #ethereum on the [Empire Hacking Slack](https://empireslacking.herokuapp.com), or email [JP Smith](mailto:jp@trailofbits.com) (the lead author) directly.

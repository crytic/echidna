# echidna

[![Build Status](https://travis-ci.org/trailofbits/echidna.svg?branch=master)](https://travis-ci.org/trailofbits/echidna)

![echidna logo](echidna.png)

Echidna is a weird creature that eats bugs and is highly electrosensitive (with apologies to Jacob Stanley)

More seriously, it's a Haskell library designed for fuzzing/property based testing of EVM code.
Currently it is quite alpha, and the API isn't guaranteed to be functional, let alone stable.
It supports relatively sophisticated grammar-based fuzzing campaigns to falsify a variety of predicates.

## Installation

[stack](https://www.haskellstack.org/) is highly recommended to install echidna.
If you are a particularly opinionated experienced Haskell user, cabal or hpack should work, but they are neither officially supported nor tested. Install stack by following stack's [installation guide](https://docs.haskellstack.org/en/stable/install_and_upgrade/).

### Build-time Dependencies

Before starting with it, make sure you have libgmp-dev installed otherwise ghc will fail to compile. Echidna or its dependencies also require the following libraries

* bzip2
* readline
* zlib
* ncurses
* secp256k1

Most of the necessary libraries are available for most linux distributions. For debian, the following command will get you most of the way there:

```
# apt-get install libgmp-dev libbz2-dev libreadline-dev zlib1g-dev libncurses5-dev
```

but it does not install secp256k1. Currently, the best way to handle this is by building libsecp256k1 from [source](https://github.com/bitcoin-core/secp256k1). The build instructions are available in the secp256k1 repo, but make sure you enable the recovery module when you configure it.

```
./configure --enable-module-recovery
```

After building the library, remember the location of the include directory which contains the secp256k1 headers, and the library artifacts. If you ran `make`, these will be `secp256k1/include` and `secp256k1/.libs`.

You must use these paths when running `stack install`/`stack build` commands below:

```
stack install --extra-include-dir=secp256k1/include --extra-lib-dir=secp256k1/.libs
```

To automatically apply those flags to all `stack build`/`stack install` commands you run, add the following lines to `~/.stack/config.yaml`:
```
extra-include-dirs:
- secp256k1/include
extra-lib-dirs:
- secp256k1/.libs
```

If you ran `make install`, these paths will be `/usr/local/include` and `/usr/local/lib` instead. If you installed the includes and libs to your systems standard paths for those things, they will be found automatically by stack.

If you have weird problems involving `readline` on MacOS, try:

```
brew install readline
brew link readline --force
export LDFLAGS=-L/usr/local/opt/readline/lib
export CPPFLAGS=-I/usr/local/opt/readline/include
stack install readline --extra-include-dirs=/usr/local/opt/readline/include --extra-lib-dirs=/usr/local/opt/readline/lib
stack install
```

### Build-time Dependencies w/ nix

If you instead wish to manage system dependencies using stack's [Nix integration](https://docs.haskellstack.org/en/stable/nix_integration/), you can skip installing system libraries, and instead use the `--nix` flag with all of stack's build commands. For example, instead of running

```
stack install
```

you would run

```
stack install --nix
```

This will tell stack to use Nix to download or build the requisite dependencies, and then run `stack install`.

### Runtime Dependencies

[solc](https://www.npmjs.com/package/solc) is another echidna dependency not handled via stack.
It is technically optional, but working with solidity source will fail without it.
Install `solc` following the [official document](https://solidity.readthedocs.io/en/v0.4.24/installing-solidity.html).
Note that `solc` must be installed by any method other than `npm / Node.js`.

Once solc is installed, you will be able to run `echidna-test` on Solidity source files.  Notably, if you are using stack, `stack ghci` will set up a REPL with all functions in scope.
This can be quite useful for playing around with the library.

## Docker Installation

Set your Docker service to allow 4GB of RAM. Build the Dockerfile with the following command

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

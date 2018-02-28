# echidna

Echidna is a weird creature that eats bugs and is highly electrosensitive (with apologies to Jacob Stanley)

More seriously, it's a Haskell library designed for fuzzing/property based testing of EVM code.
Currently it is quite alpha, and the API isn't guaranteed to be functional, let alone stable.
It supports relatively sophisticated grammar-based fuzzing campaigns to falsify a variety of predicates.

## Installation

[stack](https://www.haskellstack.org/) is highly recommended to install echidna.
If you are a particularly opinionated experienced Haskell user, cabal or hpack should work, but they are neither officially supported nor tested. 

Before starting with it, make sure you have libgmp-dev installed otherwise ghc will fail to compile. Also, libbz2 and libreadline are required by some packages. For instance, in Ubuntu/Debian you can execute:

```
# apt-get install libgmp-dev libbz2-dev libreadline-dev
```

[solc](https://www.npmjs.com/package/solc) is another echidna dependency not handled via stack.
It is technically optional, but working with solidity source will fail without it.

Once solc is installed, installing stack and running `stack upgrade; stack setup; stack install` should be all that's needed.

Notably, if you are using stack, `stack ghci` will set up a REPL with all functions in scope.
This can be quite useful for playing around with the library.

## Usage

echidna is actively being developed with more or less no regard for stability.
As a result of this, there is a lack of extensive documentation at the present time.
There is a small working example [provided](src/examples/Simple.hs) that should be relatively instructional.

### [Echidna.ABI](src/Echidna/ABI.hs)

This module provides Hedgehog generators for most of the EVM ABI.
It can be used without any other module to provide random "ASTs" (e.g. a random dynamic array of static arrays of 16 248-bit unsigned ints) or calldata (EVM-encoded function calls with these arguments).

Whenever possible, it tries to copy the convention of hevm.

### [Echidna.Exec](src/Echidna/Exec.hs)

This module provides functionality for executing fuzzing campaigns.
`fuzz` executes a fuzzing campaign to check for a given predicate.
Its arguments are annotated in the source.

Notably, the predicates are specified to be compatible with hevm's `Case` type (just use `checkExpectation`).
Should neither that nor writing a predicate by hand suffice for checking some invariant, `solPredicate` is also provided for hotloading solidity contracts with more sophisticated predicates.
`solPredicate` is currently not practical for real usage though, as it runs `solc` once per invocation.

### [Echidna.Solidity](src/Echidna/Solidity.hs)

This module provides `loadSolidity`, which takes a solidity source file and provides a VM with the first contract therein loaded as well as a `fuzz`-compatible ABI definition.
At the moment, it is only compatible with single-contract solidity files, though if given a multi-contract file it will load the first and print a warning.

## Questions/complaints/etc.

Leave an issue or shoot me a line at jp@trailofbits.com

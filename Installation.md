# Installation

## Precompiled binaries

Before starting, make sure Slither is [installed](https://github.com/crytic/slither) (`pip3 install slither-analyzer --user`).
If you want to quickly test Echidna in Linux or MacOS, we provide statically linked Linux binaries built on Ubuntu and mostly static MacOS binaries on our [releases page](https://github.com/crytic/echidna/releases). You can also grab the same type of binaries from our [CI pipeline](https://github.com/crytic/echidna/actions?query=workflow%3ACI+branch%3Amaster+event%3Apush), just click the commit to find binaries for Linux or MacOS.

## Docker container

If you prefer to use a pre-built Docker container, log into Github on your local `docker` client and check out our [docker package](https://github.com/crytic/echidna/packages/136575), which are also auto-built via Github Actions.
Otherwise, if you want to install the latest released version of Echidna, we recommend using docker:

```
$ docker build -t echidna .
```

Then, run it via:

```
$ docker run -it -v `pwd`:/src echidna echidna-test /src/examples/solidity/basic/flags.sol
```

## Building using Stack

If you'd prefer to build from source, use [Stack](https://docs.haskellstack.org/en/stable/README/). `stack install` should build and compile `echidna-test` in `~/.local/bin`. You will need to link against libreadline and libsecp256k1 (built with recovery enabled), which should be installed with the package manager of your choosing. You also need to install the latest release of [libff](https://github.com/scipr-lab/libff). Refer to our [CI tests](.github/scripts/install-libff.sh) for guidance.

Some Linux distributions do not ship static libraries for certain things that Haskell needs, e.g. Arch Linux, which will cause `stack build` to fail with linking errors because we use the `-static` flag. Removing these from `package.yaml` should get everything to build if you are not looking for a static build.

If you're getting errors building related to linking, try tinkering with `--extra-include-dirs` and `--extra-lib-dirs`.

## Building using Nix

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

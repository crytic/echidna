#!/bin/sh

set -eux

git clone --recursive https://github.com/dapphub/dapptools/
cd dapptools
nix-env -f . -iA hevm
cd src/hevm
cabal install cabal cabal-install
stack install
cd ../../..

#!/bin/sh

set -eux

git clone https://github.com/ggrieco-tob/dapptools/
cd dapptools
cd src/libethjet
sed -i "1s/.*/with import <nixpkgs> {};/" default.nix
nix-build
nix-env -i ./result
cd ../../src/hevm
nix-env -i secp256k1
#ln -s ~/.nix-profile/ nix
#LD_LIBRARY_PATH=nix/lib stack install
cd ../../..

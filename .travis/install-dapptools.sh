#!/bin/sh

set -eux

git clone https://github.com/ggrieco-tob/dapptools/
cd dapptools
#nix-env -f . -iA hevm
cd src/libethjet
sed -i "1s/.*/with import <nixpkgs> {};/" default.nix
nix-build
nix-env -i ./result
cd ../../src/hevm
nix-env -i secp256k1
ln -s ~/.nix-profile/ nix
export LD_LIBRARY_PATH=$LD_LIBRARY_PATH:nix/lib
ls nix -R
stack install
cd ../../..

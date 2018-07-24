#!/bin/sh

set -eux

git clone https://github.com/dapphub/dapptools/
cd dapptools
cd src/libethjet
sed -i.bak "s/{ stdenv, secp256k1 }:/with import <nixpkgs> {};/" default.nix
nix-env -i secp256k1
nix-env -f . -i libethjet

#!/bin/sh

set -eux

wget https://github.com/dapphub/dapptools/archive/master.zip
unzip master.zip
mv dapptools-master dapptools
cd dapptools
cd src/libethjet
sed -i.bak "s/{ stdenv, secp256k1 }:/with import <nixpkgs> {};/" default.nix
nix-env -i secp256k1
nix-env -f . -i libethjet

#!/bin/sh

set -eux

NIX=`which nix-env 2>/dev/null`

if [ "$NIX" = "" ]; then
  echo "Error: can't find 'nix-env' in your \$PATH" 1>&2
  exit 1
fi

git clone https://github.com/dapphub/dapptools/
cd dapptools
cd src/libethjet
sed -i.bak "s/{ stdenv, secp256k1 }:/with import <nixpkgs> {};/" default.nix
nix-env -i secp256k1
nix-env -f . -i libethjet
cd ../../..

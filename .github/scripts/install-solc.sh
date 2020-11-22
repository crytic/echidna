#!/bin/bash

# Adapted from https://github.com/commercialhaskell/stack

set -eux

mkdir -p $HOME/.local/bin;

travis_retry() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

fetch_solc_linux() {
  VER="$1"
  if [ ! -f "$HOME/.local/bin/solc-$VER" ]; then
    rm -Rf solc-static-linux
    wget "https://github.com/ethereum/solidity/releases/download/v$VER/solc-static-linux"
    chmod +x solc-static-linux
    mv solc-static-linux "$HOME/.local/bin/solc-$VER"
    echo "Downloaded solc $VER"
  else
    echo "Skipped solc $VER, already present"
  fi
}

fetch_all_solc_linux() {
  fetch_solc_linux "0.4.25"
  fetch_solc_linux "0.5.7"
  fetch_solc_linux "0.6.12"
  fetch_solc_linux "0.7.5"
}

if [ "$HOST_OS" = "Linux" ]; then
  if [ "${SOLC_VER:-}" == "" ]; then
    travis_retry fetch_all_solc_linux
  else
    travis_retry fetch_solc_linux "$SOLC_VER"
  fi
fi

#!/bin/bash

# Adapted from https://github.com/commercialhaskell/stack

set -eux

mkdir -p $HOME/.local/bin;

travis_retry() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

fetch_solc() {
  VER="$1"
  FILE="$2"
  EXT="$3"

  if [ ! -f "$HOME/.local/bin/solc-$VER" ]; then
    rm -Rf "$FILE"
    wget "https://github.com/ethereum/solidity/releases/download/v$VER/$FILE"
    chmod +x "$FILE"
    mv "$FILE" "$HOME/.local/bin/solc-$VER$EXT"
    echo "Downloaded solc $VER"
  else
    echo "Skipped solc $VER, already present"
  fi
}

fetch_solc_linux() {
  fetch_solc "$1" "solc-static-linux" ""
}

fetch_solc_windows() {
  fetch_solc "$1" "solc-windows.exe" ".exe"
}

fetch_all() {
  FETCHER="$1"

  "$FETCHER" "0.4.25"
  "$FETCHER" "0.5.7"
  "$FETCHER" "0.6.12"
  "$FETCHER" "0.7.5"
}

if [ "$HOST_OS" = "Linux" ]; then
  if [ "${SOLC_VER:-}" == "" ]; then
    travis_retry fetch_all fetch_solc_linux
  else
    travis_retry fetch_solc_linux "$SOLC_VER"
  fi
elif [ "$HOST_OS" = "Windows" ]; then
  if [ "${SOLC_VER:-}" == "" ]; then
    travis_retry fetch_all fetch_solc_windows
  else
    travis_retry fetch_solc_windows "$SOLC_VER"
  fi
fi
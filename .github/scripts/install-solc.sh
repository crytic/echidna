#!/bin/bash
set -eux -o pipefail

pip3 install solc-select --user

travis_retry() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

fetch_solc() {
  VER="$1"
  solc-select use "$VER" --always-install
}

fetch_all() {
  fetch_solc "0.4.25"
  fetch_solc "0.5.7"
  fetch_solc "0.6.12"
  fetch_solc "0.7.5"
}


if [ "${SOLC_VER:-}" == "" ]; then
  travis_retry fetch_all
else
  travis_retry fetch_solc "$SOLC_VER"
fi

#!/bin/sh

# Adapted from https://github.com/commercialhaskell/stack

set -eux

mkdir -p ~/.local/bin;

travis_retry() {
  cmd=$*
  $cmd || (sleep 2 && $cmd) || (sleep 10 && $cmd)
}

#fetch_stack_osx() {
#}

fetch_stack_linux() {
  rm -Rf solc-static-linux;
  curl https://github.com/ethereum/solidity/releases/download/v0.4.25/solc-static-linux;
  chmod +x solc-static-linux;
  mv solc-static-linux ~/.local/bin;
}

if [ "$(uname)" = "Darwin" ]; then
  echo travis_retry fetch_stack_osx
else
  travis_retry fetch_stack_linux
fi

solc --version

#!/bin/bash
set -eux -o pipefail

if [ "$HOST_OS" = "Linux" ]; then
  if [ $(uname -m) = "aarch64" ]; then
    curl -fsSL -o z3.zip https://github.com/Z3Prover/z3/releases/download/z3-4.15.2/z3-4.15.2-arm64-glibc-2.34.zip
  elif [ $(uname -m) = "x86_64" ]; then
    curl -fsSL -o z3.zip https://github.com/Z3Prover/z3/releases/download/z3-4.15.2/z3-4.15.2-x64-glibc-2.39.zip
  fi
  unzip z3.zip
  cp -a z3-*/bin/z3 "$HOME/.local/bin/"
  rm -rf z3-*/ z3.zip
fi
if [ "$HOST_OS" = "Windows" ]; then
  choco install z3
fi

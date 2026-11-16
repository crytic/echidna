#!/bin/bash
set -eux -o pipefail

VERSION=5.3.3

mkdir -p "$HOME/.local/bin/"

if [ "$HOST_OS" = "Linux" ]; then
  if [ "$(uname -m)" = "aarch64" ]; then
    curl -fsSL -o z3.zip "https://github.com/Z3Prover/z3/releases/download/z3-$VERSION/z3-$VERSION-arm64-glibc-2.38.zip"
  elif [ "$(uname -m)" = "x86_64" ]; then
    curl -fsSL -o z3.zip "https://github.com/Z3Prover/z3/releases/download/z3-$VERSION/z3-$VERSION-x64-glibc-2.39.zip"
  fi
  unzip z3.zip
  cp -a z3-*/bin/z3 "$HOME/.local/bin/"
  rm -rf z3-*/ z3.zip
fi
if [ "$HOST_OS" = "Windows" ]; then
  curl -fsSL -o z3.zip "https://github.com/Z3Prover/z3/releases/download/z3-$VERSION/z3-$VERSION-x64-win.zip"
  unzip z3.zip
  cp -a z3-*/bin/* "$HOME/.local/bin/"
  rm -rf z3-*/ z3.zip
fi

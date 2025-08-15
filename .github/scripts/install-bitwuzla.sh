#!/bin/bash
set -eux -o pipefail

VERSION=0.8.2

mkdir -p "$HOME/.local/bin/"

if [ "$HOST_OS" = "Linux" ]; then
  if [ $(uname -m) = "aarch64" ]; then
    curl -fsSL -o bitwuzla.zip https://github.com/bitwuzla/bitwuzla/releases/download/0.8.2/Bitwuzla-Linux-arm64-static.zip
  elif [ $(uname -m) = "x86_64" ]; then
    curl -fsSL -o bitwuzla.zip https://github.com/bitwuzla/bitwuzla/releases/download/0.8.2/Bitwuzla-Linux-x86_64-static.zip
  fi
  unzip bitwuzla.zip
  cp Bitwuzla*/bin/bitwuzla "$HOME/.local/bin/"
fi
if [ "$HOST_OS" = "Windows" ]; then
  curl -fsSL "https://github.com/bitwuzla/bitwuzla/releases/download/$VERSION/Bitwuzla-Win64-x86_64-static.zip" -o bitwuzla.zip
  unzip bitwuzla.zip
  cp Bitwuzla*/bin/bitwuzla.exe "$HOME/.local/bin/"
fi

rm -rf bitwuzla.zip Bitwuzla*/

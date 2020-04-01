#! /bin/bash

set -ex

if [ -f "$HOME/.local/bin/ghr" ]; then
  echo "found ghr, exiting..."
  exit 0
fi

GITHUB="https://github.com/tcnksm/ghr/releases/download"
VERSION="v0.13.0"

echo "Installing ghr"
mkdir -p "$HOME/.local/bin"
if [ "$HOST_OS" = "Linux" ]; then
  ARCH="linux"
  curl -L "${GITHUB}/${VERSION}/ghr_${VERSION}_${ARCH}_amd64.tar.gz" > ghr.tar.gz
  tar -xf ghr.tar.gz
elif [ "$HOST_OS" = "macOS" ]; then
  ARCH="darwin"
  curl -L "${GITHUB}/${VERSION}/ghr_${VERSION}_${ARCH}_amd64.zip" > ghr.zip
  unzip ghr.zip
else
  echo "unknown host"
  exit 1
fi

DIRNAME="ghr_${VERSION}_${ARCH}_amd64"
mv "${DIRNAME}/ghr" "$HOME/.local/bin/ghr"

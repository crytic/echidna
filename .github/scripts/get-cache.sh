#! /bin/bash

set -ex

source .github/scripts/host.sh
source .github/scripts/libs.sh

mkdir -p .github-cache
mkdir -p $HOME/.local/lib

for lib in ${LIBS[@]}; do
  if [ -f .github-cache/$lib ]; then
    mv .github-cache/$lib $HOME/.local/lib
  fi
done

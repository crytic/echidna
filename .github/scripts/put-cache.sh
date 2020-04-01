#! /bin/bash

set -ex

source .github/scripts/host.sh
source .github/scripts/libs.sh

mkdir -p .github-cache

for lib in ${LIBS[@]}; do
  mv $HOME/.local/lib/$lib .github-cache/
done

#!/bin/sh

set -eux

git clone https://github.com/dapphub/dapptools/
pushd dapptools
make install
popd

#!/bin/sh

set -eux

git clone https://github.com/dapphub/dapptools/
cd dapptools
make install
cd src/hevm
stack install
cd ../../..

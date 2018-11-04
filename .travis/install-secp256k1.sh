#!/bin/sh

set -eux

gitRef="1086fda4c1975d0cad8d3cad96794a64ec12dca4"

curl -LO "https://github.com/bitcoin-core/secp256k1/archive/$gitRef.zip"

unzip "$gitRef.zip"

cd "secp256k1-$gitRef"

./autogen.sh
# hevm needs reecovery module
# don't install shared library so linker must used static
# enable pic so static library can link against dynamic correctly
./configure --enable-module-recovery --enable-shared=no --with-pic
sudo make install

#!/bin/sh

set -eux

if ls /usr/local/lib | grep -q libsecp256k1; then exit 0; fi

gitRef="1086fda4c1975d0cad8d3cad96794a64ec12dca4"
curl -LO "https://github.com/bitcoin-core/secp256k1/archive/$gitRef.zip"

unzip "$gitRef.zip"
cd "secp256k1-$gitRef"

./autogen.sh
# hevm needs reecovery module
# enable pic so static library can link against dynamic correctly
./configure --enable-module-recovery --with-pic

sudo make install

#! /bin/bash

set -eux

source .github/scripts/host.sh

if [ -f $HOME/.local/lib/libsecp256k1.a ]; then
  echo "libsecp256k1 exists, exiting..."
  exit 0
fi

gitRef="21ffe4b22a9683cf24ae0763359e401d1284cc7a"
curl -LO "https://github.com/bitcoin-core/secp256k1/archive/$gitRef.zip"

unzip "$gitRef.zip"
cd "secp256k1-$gitRef"

./autogen.sh
# hevm needs reecovery module
# enable pic so static library can link against dynamic correctly
./configure --prefix=$PREFIX --enable-module-recovery --disable-benchmark --disable-tests --with-pic

make install

if [ "$HOST_OS" = "Windows" ]; then
  # Delete file that causes failure to link
  find $PREFIX -name libsecp256k1.dll.a -delete
fi

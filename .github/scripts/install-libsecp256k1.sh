#! /bin/bash

set -eux

source .github/scripts/host.sh

if [ -f $HOME/.local/lib/libsecp256k1.a ]; then
  echo "libsecp256k1 exists, exiting..."
  exit 0
fi

INSTALL_VERSION=0.6.0
curl -LO "https://github.com/bitcoin-core/secp256k1/archive/v$INSTALL_VERSION.zip"

unzip "v$INSTALL_VERSION.zip" && rm "v$INSTALL_VERSION.zip"
cd "secp256k1-$INSTALL_VERSION"

./autogen.sh
# hevm needs reecovery module
# enable pic so static library can link against dynamic correctly
./configure --prefix=$PREFIX --enable-module-recovery --disable-benchmark --disable-tests --with-pic

make install

if [ "$HOST_OS" = "Windows" ]; then
  # Delete file that causes failure to link
  find $PREFIX -name libsecp256k1.dll.a -delete
fi

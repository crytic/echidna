#!/bin/sh

set -eux

if ls /usr/local/lib | grep -q libff; then exit 0; fi

git clone https://github.com/scipr-lab/libff --recursive
cd libff
ARGS="-DWITH_PROCPS=OFF"
if [ "$(uname)" == "Darwin" ]; then
  brew update # not sure if this is required
  brew upgrade openssl
  ARGS="$ARGS -DOPENSSL_INCLUDE_DIR=/usr/local/opt/openssl/include/openssl"
fi
CXXFLAGS='-fPIC' cmake $ARGS .
sudo make install

#!/bin/sh

set -eux

if ls /usr/local/lib | grep -q libff; then exit 0; fi

git clone https://github.com/scipr-lab/libff --recursive
cd libff
ARGS="-DWITH_PROCPS=OFF"
if [ "$(uname)" == "Darwin" ]; then
  export LDFLAGS=-L/usr/local/opt/openssl/lib
  export CPPFLAGS=-I/usr/local/opt/openssl/include
  export CXXFLAGS=-I/usr/local/opt/openssl/include
  ARGS="$ARGS -DOPENSSL_INCLUDE_DIR=/usr/local/opt/openssl/include/openssl"
fi
CXXFLAGS="-fPIC $CXXFLAGS" cmake $ARGS .
sudo make install

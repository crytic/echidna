#!/bin/sh

set -eux

if ls /usr/local/lib | grep -q libff; then exit 0; fi

git clone https://github.com/scipr-lab/libff --recursive
git submodule init && git submodule update
cd libff
ARGS="-DWITH_PROCPS=OFF"
if [ "$(uname)" == "Darwin" ]; then
  export LDFLAGS=-L/usr/local/opt/openssl/lib
  export CPPFLAGS=-I/usr/local/opt/openssl/include
  export CXXFLAGS=-I/usr/local/opt/openssl/include
  ARGS="$ARGS -DOPENSSL_INCLUDE_DIR=/usr/local/opt/openssl/include/openssl -DCMAKE_CXX_STANDARD=14 -DCMAKE_CXX_FLAGS=\“-D_FILE_OFFSET_BITS=64 -DMIE_ATE_USE_GMP -DNDEBUG\""
fi
mkdir build
cd build
CXXFLAGS="-fPIC $CXXFLAGS" cmake $ARGS ..
make && sudo make install

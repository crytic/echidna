#! /bin/bash

set -eux

source .github/scripts/host.sh

if [ -f $HOME/.local/lib/libff"$EXT" ]; then
  echo "libff exists, exiting..."
  exit 0
fi

git clone https://github.com/scipr-lab/libff --recursive
cd libff
git submodule init && git submodule update
git checkout v0.2.1

ARGS="-DCMAKE_INSTALL_PREFIX=$PREFIX -DWITH_PROCPS=OFF"
CXXFLAGS=""
if [ "$HOST_OS" = "macOS" ]; then
  OPENSSL_PREFIX=$(brew --prefix openssl)
  export LDFLAGS=-L$OPENSSL_PREFIX/lib
  export CPPFLAGS=-I$OPENSSL_PREFIX/include
  export CXXFLAGS=-I$OPENSSL_PREFIX/include
  ARGS="$ARGS -DOPENSSL_INCLUDE_DIR=$OPENSSL_PREFIX/include/openssl -DCURVE=ALT_BN128 -DCMAKE_INSTALL_NAME_DIR=$PREFIX/lib"
  sed -i '' 's/STATIC/SHARED/' libff/CMakeLists.txt # Fix GHC segfaults from hell (idk why)
  sed -i '' 's/STATIC/SHARED/' depends/CMakeLists.txt
fi

mkdir build
cd build
CXXFLAGS="-fPIC $CXXFLAGS" cmake $ARGS ..
make && make install

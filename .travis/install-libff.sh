#!/bin/sh

set -eux

if ls /usr/local/lib | grep -q libff; then exit 0; fi

#gitRef="1086fda4c1975d0cad8d3cad96794a64ec12dca4"

git clone https://github.com/scipr-lab/libff --recursive
cd libff
CXXFLAGS='-fPIC' cmake .
sudo make install

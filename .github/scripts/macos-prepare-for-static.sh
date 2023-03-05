#! /bin/bash

set -eux

source .github/scripts/host.sh

if [ "$HOST_OS" = "macOS" ]; then
  # Delete dynamic libraries so GHC prefers to link against static ones
  find "$PREFIX" -name '*.dylib' -delete
fi

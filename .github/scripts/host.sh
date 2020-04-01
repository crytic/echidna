#! /bin/bash

if [ -v PREFIX ]; then
else
  export PREFIX="$HOME/.local"
fi

if [ "$HOST_OS" = "Linux" ]; then
  EXT=".a"
elif [ "$HOST_OS" = "macOS" ]; then
  EXT=".dylib"
else
  echo "unrecognized host"
  exit 1
fi

#! /bin/bash

if [ -z ${PREFIX+x} ]; then
  export PREFIX="$HOME/.local"
fi

if [ -z ${HOST_OS+x} ]; then
  export HOST_OS=`uname`
  if [ "$HOST_OS" = "Darwin" ]; then
    export HOST_OS="macOS"
  elif [[ "$HOST_OS" == MINGW* ]]; then
    export HOST_OS="Windows"
  fi 
fi

if [ "$HOST_OS" = "Linux" ]; then
  EXT=".a"
elif [ "$HOST_OS" = "macOS" ]; then
  EXT=".dylib"
elif [ "$HOST_OS" = "Windows" ]; then
  EXT=".a"
else
  echo "unrecognized host"
  exit 1
fi

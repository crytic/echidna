#!/bin/bash
set -eux -o pipefail

if [ "$HOST_OS" = "Linux" ]; then
  sudo apt-get update
  sudo apt-get install -y z3
fi
if [ "$HOST_OS" = "Windows" ]; then
  choco install z3
fi

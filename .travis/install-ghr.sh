#!/bin/sh
set -o errexit -o verbose

if [ "$TRAVIS_OS_NAME" = "linux" ] 
then
    ARCH="linux"
else
    ARCH="darwin"
fi

echo "Installing ghr"
URL="https://github.com/tcnksm/ghr/releases/download/v0.5.4/ghr_v0.5.4_${ARCH}_386.zip"
curl -L ${URL} > ghr.zip
mkdir -p "$HOME/bin"
export PATH="$HOME/bin:$PATH"
unzip ghr.zip -d "$HOME/bin"
rm ghr.zip

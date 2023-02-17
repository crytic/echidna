#!/bin/bash
set -eux

copy_dll()
{
    find /ucrt64 -name "$1-*.dll" -exec cp {} "$2" \;
}

BUILD="$(mktemp -d)/echidna"
mkdir -p "$BUILD"
cp "$APPDATA/local/bin/echidna.exe" "$BUILD"

copy_dll "libgcc_s_seh" "$BUILD"
copy_dll "libstdc++" "$BUILD"
copy_dll "libgmp" "$BUILD"
copy_dll "libwinpthread" "$BUILD"

GZIP=-9 tar -czf echidna.tar.gz -C "$BUILD/.." echidna

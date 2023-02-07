#!/bin/bash
set -eux

add_rpath()
{
    BINARY="$1"
    install_name_tool -add_rpath "@executable_path/." "$BINARY"
}

fix_path()
{
    BINARY="$1"
    MATCH="$2"
    NEW="$3"
    OLD=$(otool -L "$BINARY" | grep "${MATCH}\." | awk '{print $1}')
    install_name_tool -change "$OLD" "$NEW" "$BINARY"
    cp -n "$OLD" "$(dirname "$BINARY")/$(basename "$NEW")" || true
}


BUILD="$(mktemp -d)/echidna"
mkdir -p "$BUILD"
cp "$HOME/.local/bin/echidna" "$BUILD"

BINARY="$BUILD/echidna"
add_rpath "$BINARY"
fix_path "$BINARY" libsecp256k1 "@rpath/libsecp256k1.dylib"
fix_path "$BINARY" libff "@rpath/libff.dylib"
fix_path "$BUILD/libff.dylib" libgmp "@rpath/libgmp.dylib"
fix_path "$BUILD/libsecp256k1.dylib" libgmp "@rpath/libgmp.dylib"

GZIP=-9 tar -czf echidna.tar.gz -C "$BUILD/.." echidna

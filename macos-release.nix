{ tests ? false }:
let
  pkgs = import nix/pkgs.nix;
  echidna = import ./. { inherit tests; };
in
  with pkgs; runCommand "echidna-${echidna.version}-bundled-dylibs" {
    buildInputs = [
      macdylibbundler
      darwin.sigtool
      darwin.cctools
    ];
  } ''
    mkdir -p $out/bin
    cp ${echidna}/bin/echidna-test $out/bin/echidna-test
    chmod 755 $out/bin/echidna-test
    dylibbundler -b \
      -x $out/bin/echidna-test \
      -d $out/bin \
      -p '@executable_path'

    # Manually fix iconv dylib ignored by dylibbundler
    cp ${pkgs.libiconv.outPath}/lib/libiconv-nocharset.dylib $out/bin/
    cp ${pkgs.libiconv.outPath}/lib/libcharset.1.0.0.dylib $out/bin/libcharset.1.dylib
    chmod 755 $out/bin/libiconv-nocharset.dylib $out/bin/libcharset.1.dylib
    install_name_tool -id "@rpath/libiconv-nocharset.dylib" $out/bin/libiconv-nocharset.dylib
    install_name_tool -id "@rpath/libcharset.1.dylib" $out/bin/libcharset.1.dylib
    install_name_tool -change ${pkgs.libiconv.outPath}/lib/libiconv-nocharset.dylib "@executable_path/libiconv-nocharset.dylib" $out/bin/libiconv.dylib
    install_name_tool -change ${pkgs.libiconv.outPath}/lib/libcharset.1.dylib "@executable_path/libcharset.1.dylib" $out/bin/libiconv.dylib

    # re-sign the binaries since the load paths were modified
    codesign -s - -f $out/bin/*
    tar -czvf $out/echidna-${echidna.version}-${stdenv.system}.tar.gz -C $out/bin .
  ''

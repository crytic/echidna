{ tests ? false }:
let
  pkgs = import (builtins.fetchTarball {
    name = "nixpkgs-unstable-2022-02-10";
    url = "https://github.com/nixos/nixpkgs/archive/1882c6b7368fd284ad01b0a5b5601ef136321292.tar.gz";
    sha256 = "sha256:0zg7ak2mcmwzi2kg29g4v9fvbvs0viykjsg2pwaphm1fi13s7s0i";
  }) {};
  echidna = import ./. { inherit tests; };
in
  with pkgs; runCommand "echidna-${echidna.version}-bundled-dylibs" {
    buildInputs = [
      macdylibbundler
      darwin.sigtool
      darwin.cctools
      perl
    ];
  } ''
    mkdir -p $out/bin
    cp ${echidna}/bin/echidna-test $out/bin/echidna-test
    chmod 755 $out/bin/echidna-test
    dylibbundler -b \
      -x $out/bin/echidna-test \
      -d $out/bin \
      -p '@executable_path'

    # fix TERMINFO path in ncurses binary
    perl -i -pe 's#(${pkgs.ncurses}/share/terminfo)#"/usr/share/terminfo" . "\x0" x (length($1) - 19)#e' $out/bin/libncursesw.6.dylib

    # re-sign the binaries since the load paths were modified
    codesign -s - -f $out/bin/*
    tar -czvf $out/echidna-${echidna.version}-${stdenv.system}.tar.gz -C $out/bin .
  ''

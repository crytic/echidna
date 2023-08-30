{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nix-bundle-exe = {
      url = "github:3noch/nix-bundle-exe";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, nix-bundle-exe, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        systemPkgs = nixpkgs.legacyPackages.${system};
        # prefer musl on Linux, static glibc + threading does not work properly
        # TODO: maybe only override it for echidna-redistributable?
        pkgs = if systemPkgs.stdenv.hostPlatform.isLinux then systemPkgs.pkgsMusl else systemPkgs;
        # this is not perfect for development as it hardcodes solc to 0.5.7, test suite runs fine though
        # would be great to integrate solc-select to be more flexible, improve this in future
        solc = pkgs.stdenv.mkDerivation {
          name = "solc";
          src = if pkgs.stdenv.isDarwin then
            pkgs.fetchurl {
              url = "https://binaries.soliditylang.org/macosx-amd64/solc-macosx-amd64-v0.5.7+commit.6da8b019";
              sha256 = "095mlw5x9lpdcdl9jzlvkvw46ag03xr4nj4vly4hgn92rgivimm7";
            }
          else
            pkgs.fetchurl {
              url = "https://binaries.soliditylang.org/linux-amd64/solc-linux-amd64-v0.5.7+commit.6da8b019";
              sha256 = "0dsvzck5jh8rvdxs7zyn2ga9hif024msx8gr8ifgj4cmyb7m4341";
            };
          phases = ["installPhase" "patchPhase"];
          installPhase = ''
            mkdir -p $out/bin
            cp $src $out/bin/solc
            chmod +x $out/bin/solc
          '';
        };

        secp256k1-static = stripDylib (pkgs.secp256k1.overrideAttrs (attrs: {
          configureFlags = attrs.configureFlags ++ [ "--enable-static" ];
        }));

        ncurses-static = pkgs.ncurses.override { enableStatic = true; };

        hevm = pkgs.haskell.lib.dontCheck (
          pkgs.haskellPackages.callCabal2nix "hevm" (pkgs.fetchFromGitHub {
            owner = "elopez";
            repo = "hevm";
            rev = "release/0.51.3-plus-ghc-9.4-support";
            sha256 = "sha256-gJMFYfsPqf5XZyyPDGJLqr9q9RpXkemGeUQUvFT6V0E";
        }) { secp256k1 = pkgs.secp256k1; });

        # FIXME: figure out solc situation, it conflicts with the one from
        # solc-select that is installed with slither, disable tests in the meantime
        echidna = pkgs.haskell.lib.dontCheck (
          with pkgs; lib.pipe
          (haskellPackages.callCabal2nix "echidna" ./. { inherit hevm; })
          [
            (haskell.lib.compose.addTestToolDepends [ haskellPackages.hpack slither-analyzer solc ])
            (haskell.lib.compose.disableCabalFlag "static")
          ]);

        echidna-static = with pkgs; lib.pipe
          echidna
          [
            (haskell.lib.compose.appendConfigureFlags
              ([
                "--extra-lib-dirs=${stripDylib (gmp.override { withStatic = true; })}/lib"
                "--extra-lib-dirs=${stripDylib secp256k1-static}/lib"
                "--extra-lib-dirs=${stripDylib (libff.override { enableStatic = true; })}/lib"
                "--extra-lib-dirs=${zlib.static}/lib"
                "--extra-lib-dirs=${stripDylib (libffi.overrideAttrs (_: { dontDisableStatic = true; }))}/lib"
                "--extra-lib-dirs=${stripDylib (ncurses-static)}/lib"
              ] ++ (if stdenv.hostPlatform.isDarwin then [
                "--extra-lib-dirs=${stripDylib (libiconv.override { enableStatic = true; })}/lib"
              ] else [])))
            (haskell.lib.compose.enableCabalFlag "static")
          ];

        # "static" binary for distribution
        # on linux this is actually a real fully static binary
        # on macos this has everything except libcxx and libsystem
        # statically linked. we can be confident that these two will always
        # be provided in a well known location by macos itself.
        echidnaRedistributable = let
          grep = "${pkgs.gnugrep}/bin/grep";
          perl = "${pkgs.perl}/bin/perl";
          otool = "${pkgs.darwin.binutils.bintools}/bin/otool";
          install_name_tool = "${pkgs.darwin.binutils.bintools}/bin/install_name_tool";
          codesign_allocate = "${pkgs.darwin.binutils.bintools}/bin/codesign_allocate";
          codesign = "${pkgs.darwin.sigtool}/bin/codesign";
        in if pkgs.stdenv.isLinux
        then pkgs.haskell.lib.dontCheck echidna-static
        else pkgs.runCommand "echidna-stripNixRefs" {} ''
          mkdir -p $out/bin
          cp ${pkgs.haskell.lib.dontCheck echidna-static}/bin/echidna $out/bin/
          # get the list of dynamic libs from otool and tidy the output
          libs=$(${otool} -L $out/bin/echidna | tail -n +2 | sed 's/^[[:space:]]*//' | cut -d' ' -f1)
          # get the path for libcxx
          cxx=$(echo "$libs" | ${grep} '^/nix/store/.*-libcxx')
          # rewrite /nix/... library paths to point to /usr/lib
          chmod 777 $out/bin/echidna
          ${install_name_tool} -change "$cxx" /usr/lib/libc++.1.dylib $out/bin/echidna
          # fix TERMINFO path in ncurses
          ${perl} -i -pe 's#(${ncurses-static}/share/terminfo)#"/usr/share/terminfo" . "\x0" x (length($1) - 19)#e' $out/bin/echidna
          # re-sign binary
          CODESIGN_ALLOCATE=${codesign_allocate} ${codesign} -f -s - $out/bin/echidna
          chmod 555 $out/bin/echidna
        '';

        # if we pass a library folder to ghc via --extra-lib-dirs that contains
        # only .a files, then ghc will link that library statically instead of
        # dynamically (even if --enable-executable-static is not passed to cabal).
        # we use this trick to force static linking of some libraries on macos.
        stripDylib = drv : pkgs.runCommand "${drv.name}-strip-dylibs" {} ''
          mkdir -p $out
          mkdir -p $out/lib
          cp -r ${drv}/* $out/
          rm -rf $out/**/*.dylib
        '';

      in rec {
        packages.echidna = echidna;
        packages.default = echidna;

        packages.echidna-redistributable = echidnaRedistributable;

        devShell = with pkgs;
          haskellPackages.shellFor {
            packages = _: [ echidna ];
            shellHook = "hpack";
            buildInputs = [
              solc
              haskellPackages.hlint
              haskellPackages.cabal-install
              haskellPackages.haskell-language-server
            ];
            withHoogle = true;
          };
      }
    );
}

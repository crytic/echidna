{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    foundry = {
      url = "github:shazow/foundry.nix/stable";
      inputs.nixpkgs.follows = "nixpkgs";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    solc-pkgs = {
      url = "github:hellwolf/solc.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, solc-pkgs, foundry, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            solc-pkgs.overlay
            foundry.overlay
            (final: prev: {
              # build with GHC 9.8
              haskellPackages = prev.haskell.packages.ghc98.override {
                overrides = hfinal: hprev: {
                  # Bump tls to >= 2.2.2 for the fix in https://github.com/haskell-tls/hs-tls/pull/515.
                  # tls 2.2.2 needs the crypton-x509 1.8 line, which still builds against the
                  # crypton 1.0.x already in nixpkgs (the 1.9 line would force crypton >= 1.1 and a
                  # much wider cascade). Everything else it needs (crypton-asn1-*, crypton-pem,
                  # hpke, ech-config, time-hourglass) is already at a compatible version, and the
                  # rest of the package set rebuilds against these through the fixpoint.
                  tls = hfinal.callHackageDirect {
                    pkg = "tls"; ver = "2.2.2";
                    sha256 = "sha256-lbroDPZiOa2YH1jqEzxzNgBGcPZDP6WJEeD0odDgNqs=";
                  } {};
                  crypton-x509 = hfinal.callHackageDirect {
                    pkg = "crypton-x509"; ver = "1.8.0";
                    sha256 = "sha256-wxU8Ou52UCuCT2gbxqPKssteIVGUyg5WEbv1xRIyZTg=";
                  } {};
                  crypton-x509-store = hfinal.callHackageDirect {
                    pkg = "crypton-x509-store"; ver = "1.8.0";
                    sha256 = "sha256-U6DH5Ke3JXAzZuqxLM6mPKDxqj4HTf5kjoBXaerLOcc=";
                  } {};
                  crypton-x509-validation = hfinal.callHackageDirect {
                    pkg = "crypton-x509-validation"; ver = "1.8.0";
                    sha256 = "sha256-CyRqTUOcUzzVlQfTd3yylwDVtOaumBbBg9hMyvtcu7c=";
                  } {};

                  # Relax the `tls < 2.2` upper bounds of the other tls consumers in the set
                  # (crypton-connection pulled in by echidna; warp-tls/tls-session-manager pulled
                  # in by Hoogle in the dev shell). All compile fine against 2.2.2.
                  crypton-connection = prev.haskell.lib.doJailbreak hprev.crypton-connection;
                  warp-tls = prev.haskell.lib.doJailbreak hprev.warp-tls;
                  tls-session-manager = prev.haskell.lib.doJailbreak hprev.tls-session-manager;

                  # callHackageDirect runs the `cabal2nix` tool from this set, and cabal2nix
                  # transitively depends on tls — regenerating tls with it would loop. Pin the
                  # tool to the un-overridden base set to break the cycle (it's build-time only,
                  # so its own tls version is irrelevant).
                  cabal2nix = prev.haskell.packages.ghc98.cabal2nix;
                };
              };
            })
          ];
        };

        # prefer musl on Linux, static glibc + threading does not work properly
        # TODO: maybe only override it for echidna-redistributable?
        pkgsGHC = if pkgs.stdenv.hostPlatform.isLinux then pkgs.pkgsMusl else pkgs;
        pkgsDeps = if pkgs.stdenv.hostPlatform.isLinux then pkgs.pkgsStatic else pkgs;
        # this is not perfect for development as it hardcodes solc to 0.5.7, test suite runs fine though
        # 0.5.7 is not available on aarch64 darwin so alternatively pick 0.8.5
        solc = solc-pkgs.mkDefault pkgs (pkgs.solc_0_5_7 or pkgs.solc_0_8_5);

        dependencies-static = with pkgsDeps; [
          (gmp.override { withStatic = true; })
          (pkgsDeps.secp256k1.overrideAttrs (attrs: {
            configureFlags = attrs.configureFlags ++ [ "--enable-static" ];
          }))
          (libff.override { enableStatic = true; })
          (ncurses.override { enableStatic = true; })
        ] ++ lib.optionals (pkgs.stdenv.hostPlatform.isLinux && pkgs.stdenv.hostPlatform.isx86_64) [
          # FIXME: work around wrong libdw / libelf linking on musl builds on x86_64
          (lib.getLib xz)
          (lib.getLib bzip2)
          (lib.getLib zstd)
        ] ++ lib.optionals (!pkgs.stdenv.hostPlatform.isDarwin) [
          # darwin provides these
          (zlib.override { static = true; shared = false; })
          (libffi.overrideAttrs (_: { dontDisableStatic = true; }))
          (lib.getLib numactl)
        ];

        hevm = pkgs: pkgs.lib.pipe 
          (pkgs.haskellPackages.callCabal2nix "hevm" (pkgs.fetchFromGitHub {
            owner = "argotorg";
            repo = "hevm";
            rev = "4ca42fd5b2dd1344b7596775fe5fe6ac2d03021f";
            sha256 = "sha256-Gk1hroTuQuS4blEX4M4wJ7tDNg+hnMHSwtdFKpv0Gyc=";
          }) { secp256k1 = pkgs.secp256k1; })
          ([
            pkgs.haskell.lib.compose.dontCheck
          ]);

        echidna = pkgs: with pkgs; lib.pipe
          (haskellPackages.callCabal2nix "echidna" ./. { hevm = hevm pkgs; })
          ([
            # FIXME: figure out solc situation, it conflicts with the one from
            # solc-select that is installed with slither, disable tests in the meantime
            haskell.lib.compose.dontCheck
            (haskell.lib.compose.addTestToolDepends [ haskellPackages.hpack slither-analyzer solc ])
            (haskell.lib.compose.disableCabalFlag "static")
          ]);

        echidna-static = with pkgsGHC; lib.pipe
          (echidna pkgsGHC)
          ([
            (haskell.lib.compose.appendConfigureFlags
              (map (drv: "--extra-lib-dirs=${stripDylib drv}/lib") dependencies-static))
            (haskell.lib.compose.enableCabalFlag "static")
          ] ++ lib.optionals (pkgs.stdenv.hostPlatform.isLinux && pkgs.stdenv.hostPlatform.isx86_64) [
            # FIXME: work around wrong libdw / libelf linking on musl builds on x86_64
            (haskell.lib.compose.appendConfigureFlags [
              "--ghc-option=-optl-Wl,--start-group"
              "--ghc-option=-optl-lelf"
              "--ghc-option=-optl-ldw"
              "--ghc-option=-optl-lzstd"
              "--ghc-option=-optl-lz" 
              "--ghc-option=-optl-lbz2"
              "--ghc-option=-optl-llzma"
              "--ghc-option=-optl-Wl,--end-group"
            ])
          ]);

        # "static" binary for distribution
        # on linux this is actually a real fully static binary
        # on macos this has everything except libcxx and libsystem
        # statically linked. we can be confident that these two will always
        # be provided in a well known location by macos itself.
        echidnaRedistributable = let
          grep = "${pkgs.gnugrep}/bin/grep";
          otool = "${pkgs.darwin.binutils.bintools}/bin/otool";
          install_name_tool = "${pkgs.darwin.binutils.bintools}/bin/install_name_tool";
          codesign_allocate = "${pkgs.darwin.binutils.bintools}/bin/codesign_allocate";
          codesign = "${pkgs.darwin.sigtool}/bin/codesign";
        in if pkgs.stdenv.isDarwin
        then pkgs.runCommand "echidna-stripNixRefs" {} ''
          mkdir -p $out/bin
          cp ${pkgs.haskell.lib.dontCheck echidna-static}/bin/echidna $out/bin/
          # rewrite /nix/... library paths to point to /usr/lib
          exe="$out/bin/echidna"
          chmod 777 "$exe"
          for lib in $(${otool} -L "$exe" | awk '/nix\/store/{ print $1 }'); do
            case "$lib" in
              *libc++.*.dylib)    ${install_name_tool} -change "$lib" /usr/lib/libc++.dylib     "$exe" ;;
              *libc++abi.*.dylib) ${install_name_tool} -change "$lib" /usr/lib/libc++abi.dylib  "$exe" ;;
              *libffi.*.dylib)    ${install_name_tool} -change "$lib" /usr/lib/libffi.dylib     "$exe" ;;
              *libiconv.2.dylib)  ${install_name_tool} -change "$lib" /usr/lib/libiconv.2.dylib "$exe" ;;
              *libz.dylib)        ${install_name_tool} -change "$lib" /usr/lib/libz.dylib       "$exe" ;;
            esac
          done
          # check that no nix deps remain
          nixdeps=$(${otool} -L "$exe" | tail -n +2 | { ${grep} /nix/store -c || test $? = 1; })
          if [ ! "$nixdeps" = "0" ]; then
            echo "Nix deps remain in redistributable binary!"
            exit 255
          fi
          # re-sign binary
          CODESIGN_ALLOCATE=${codesign_allocate} ${codesign} -f -s - "$exe"
          chmod 555 "$exe"
        '' else echidna-static;

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
        packages.echidna = echidna pkgs;
        packages.default = echidna pkgs;

        packages.echidna-redistributable = echidnaRedistributable;

        devShells = with pkgs; {
          default = haskellPackages.shellFor {
            packages = _: [ (echidna pkgs) ];
            shellHook = ''
              hpack
            '';
            buildInputs = [
              libff
              secp256k1
              solc
              slither-analyzer
              haskellPackages.hlint
              haskellPackages.cabal-install
              haskellPackages.haskell-language-server
            ];
            withHoogle = true;
          };

          fuzz = mkShell {
            packages = [
              (echidna pkgs)
              slither-analyzer
              foundry-bin
              bitwuzla
              cvc5
              z3
            ];
          };
        };
      }
    );
}

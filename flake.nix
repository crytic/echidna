{
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    foundry = {
      url = "github:shazow/foundry.nix/stable";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    solc-pkgs = {
      url = "github:hellwolf/solc.nix";
      inputs.nixpkgs.follows = "nixpkgs";
      inputs.flake-utils.follows = "flake-utils";
    };
  };

  outputs =
    {
      nixpkgs,
      flake-utils,
      solc-pkgs,
      foundry,
      ...
    }:
    flake-utils.lib.eachDefaultSystem (
      system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [
            solc-pkgs.overlay
            foundry.overlay
          ];
        };

        # Select GHC for Echidna without changing the native build tools.
        hspkgs =
          ps:
          ps.haskell.packages.ghc912.override {
            overrides = hfinal: hprev: {
              # GHC 9.12 changes the generated Core for three inspection tests
              # in optics 0.4.2.1. Keep the remaining tests enabled.
              optics = ps.haskell.lib.overrideCabal hprev.optics (old: {
                testFlags = (old.testFlags or [ ]) ++ [
                  "--pattern=!/traverseOf_ (_Left % itraversed % _1 % ifolded)/ && !/iover (itraversed..itraversed)/ && !/iset (itraversed..)/"
                ];
              });

              # Bump tls to >= 2.2.2 for the fix in https://github.com/haskell-tls/hs-tls/pull/515.
              # tls 2.2.2 needs the crypton-x509 1.8 line, which still builds against the
              # crypton 1.0.x already in nixpkgs (the 1.9 line would force crypton >= 1.1 and a
              # much wider cascade). Everything else it needs (crypton-asn1-*, crypton-pem,
              # hpke, ech-config, time-hourglass) is already at a compatible version, and the
              # rest of the package set rebuilds against these through the fixpoint.
              tls = hfinal.callHackageDirect {
                pkg = "tls";
                ver = "2.2.2";
                sha256 = "sha256-lbroDPZiOa2YH1jqEzxzNgBGcPZDP6WJEeD0odDgNqs=";
              } { };
              crypton-x509 = hfinal.callHackageDirect {
                pkg = "crypton-x509";
                ver = "1.8.0";
                sha256 = "sha256-wxU8Ou52UCuCT2gbxqPKssteIVGUyg5WEbv1xRIyZTg=";
              } { };
              crypton-x509-store = hfinal.callHackageDirect {
                pkg = "crypton-x509-store";
                ver = "1.8.0";
                sha256 = "sha256-U6DH5Ke3JXAzZuqxLM6mPKDxqj4HTf5kjoBXaerLOcc=";
              } { };
              crypton-x509-validation = hfinal.callHackageDirect {
                pkg = "crypton-x509-validation";
                ver = "1.8.0";
                sha256 = "sha256-CyRqTUOcUzzVlQfTd3yylwDVtOaumBbBg9hMyvtcu7c=";
              } { };

              # Relax the `tls < 2.2` upper bounds of the other tls consumers in the set
              # (crypton-connection pulled in by echidna; warp-tls/tls-session-manager pulled
              # in by Hoogle in the dev shell). All compile fine against 2.2.2.
              crypton-connection = ps.haskell.lib.doJailbreak hprev.crypton-connection;
              warp-tls = ps.haskell.lib.doJailbreak hprev.warp-tls;
              tls-session-manager = ps.haskell.lib.doJailbreak hprev.tls-session-manager;

              # nixpkgs ships mcp-server 0.1.0.19, which answers a
              # Streamable HTTP notification with `200 {}` and a
              # server-stream GET with a discovery document. 0.2.0.1 is the
              # release that follows the spec (202 with no body, and a real
              # per-request SSE stream). It needs http-types >= 0.12.6 —
              # only that version re-exports `hOrigin` from the umbrella
              # `Network.HTTP.Types` module, which mcp-server imports
              # unqualified — and http-types has to move for the whole set,
              # since wai and warp exchange its `Status` type with
              # mcp-server. Everything in the closure accepts 0.12.6.
              http-types = hfinal.callHackageDirect {
                pkg = "http-types";
                ver = "0.12.6";
                sha256 = "sha256-bGrVUTZnP1NFVwR0apgNLolKPW3cJdVW4jSBSlV7srg=";
              } { };
              mcp-server = hfinal.callHackageDirect {
                pkg = "mcp-server";
                ver = "0.2.0.1";
                sha256 = "sha256-Pe0Jdfor8p5Iwj3oV2bNC5SpjHMqEwzMYUpqrvmgvP0=";
              } { };
            };
          };

        withStaticInterpreter = import ./nix/static-interpreter.nix {
          inherit pkgs;
          buildHaskellPackages = hspkgs pkgs;
          hostHaskellPackages = hspkgs pkgs.pkgsStatic;
        };

        # this is not perfect for development as it hardcodes solc to 0.5.7, test suite runs fine though
        # 0.5.7 is not available on aarch64 darwin so alternatively pick 0.8.5
        solc = solc-pkgs.mkDefault pkgs (pkgs.solc_0_5_7 or pkgs.solc_0_8_5);

        dependencies-static-macos = with pkgs; [
          (gmp.override { withStatic = true; })
          (secp256k1.overrideAttrs (attrs: {
            configureFlags = attrs.configureFlags ++ [ "--enable-static" ];
          }))
          (libff.override { enableStatic = true; })
          (ncurses.override { enableStatic = true; })
        ];

        hevm =
          pkgs:
          pkgs.lib.pipe
            ((hspkgs pkgs).callCabal2nix "hevm" (pkgs.fetchFromGitHub {
              owner = "argotorg";
              repo = "hevm";
              rev = "05ce440e6fcf33fd7389503c1ed35633cbb898ac";
              sha256 = "sha256-mhMn8bqdBqG0y3DuPJcOhuy6za5CpLP0aCigNdx4AcQ=";
            }) { secp256k1 = pkgs.secp256k1; })
            (
              [
                pkgs.haskell.lib.compose.dontCheck
                (pkgs.haskell.lib.compose.appendConfigureFlag "--enable-optimization=2")
              ]
              ++ pkgs.lib.optionals pkgs.stdenv.hostPlatform.isStatic [
                # Profiling hevm can crash the musl external interpreter.
                pkgs.haskell.lib.compose.disableLibraryProfiling
                withStaticInterpreter
              ]
            );

        echidna =
          pkgs:
          with pkgs;
          lib.pipe ((hspkgs pkgs).callCabal2nix "echidna" ./. { hevm = hevm pkgs; }) ([
            # FIXME: figure out solc situation, it conflicts with the one from
            # solc-select that is installed with slither, disable tests in the meantime
            haskell.lib.compose.dontCheck
            (haskell.lib.compose.addTestToolDepends [
              haskellPackages.hpack
              slither-analyzer
              solc
            ])
            (haskell.lib.compose.disableCabalFlag "static")
          ]);

        echidna-static =
          with pkgs;
          if stdenv.hostPlatform.isLinux then
            lib.pipe (echidna pkgs.pkgsStatic) [
              # Match hevm's release build; profiling libraries are not needed.
              haskell.lib.compose.disableLibraryProfiling
              (haskell.lib.compose.enableCabalFlag "static")
              withStaticInterpreter
            ]
          else
            lib.pipe (echidna pkgs) [
              (haskell.lib.compose.appendConfigureFlags (
                map (drv: "--extra-lib-dirs=${stripDylib drv}/lib") dependencies-static-macos
              ))
            ];

        # "static" binary for distribution
        # on linux this is actually a real fully static binary
        # on macos this has everything except libcxx and libsystem
        # statically linked. we can be confident that these two will always
        # be provided in a well known location by macos itself.
        echidnaRedistributable =
          let
            grep = "${pkgs.gnugrep}/bin/grep";
            otool = "${pkgs.darwin.binutils.bintools}/bin/otool";
            install_name_tool = "${pkgs.darwin.binutils.bintools}/bin/install_name_tool";
            codesign_allocate = "${pkgs.darwin.binutils.bintools}/bin/codesign_allocate";
            codesign = "${pkgs.darwin.sigtool}/bin/codesign";
          in
          if pkgs.stdenv.hostPlatform.isDarwin then
            pkgs.runCommand "echidna-stripNixRefs" { } ''
              mkdir -p $out/bin
              cp ${echidna-static}/bin/echidna $out/bin/
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
            ''
          else
            echidna-static;

        # if we pass a library folder to ghc via --extra-lib-dirs that contains
        # only .a files, then ghc will link that library statically instead of
        # dynamically (even if --enable-executable-static is not passed to cabal).
        # we use this trick to force static linking of some libraries on macos.
        stripDylib =
          drv:
          pkgs.runCommand "${drv.name}-strip-dylibs" { } ''
            mkdir -p $out/lib
            cp -r ${drv}/* $out/
            rm -rf $out/**/*.dylib
          '';

      in
      {
        packages.echidna = echidna pkgs;
        packages.default = echidna pkgs;

        packages.echidna-redistributable = echidnaRedistributable;

        devShells = with pkgs; {
          default = (hspkgs pkgs).shellFor {
            # Include test dependencies even though package builds skip the suite.
            packages = _: [ (haskell.lib.doCheck (echidna pkgs)) ];
            shellHook = ''
              # Prefer the pinned compiler over Slither's solc-select wrapper.
              export PATH="${solc}/bin:$PATH"
              hpack
            '';
            buildInputs = [
              libff
              secp256k1
              solc
              slither-analyzer
              (hspkgs pkgs).hlint
              (hspkgs pkgs).cabal-install
              (hspkgs pkgs).haskell-language-server
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

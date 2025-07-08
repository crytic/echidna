{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
    foundry.url = "github:shazow/foundry.nix/47f8ae49275eeff9bf0526d45e3c1f76723bb5d3";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nix-bundle-exe = {
      url = "github:3noch/nix-bundle-exe";
      flake = false;
    };
    solc-pkgs = {
      url = "github:hellwolf/solc.nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, nix-bundle-exe, solc-pkgs, foundry, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs {
          inherit system;
          overlays = [solc-pkgs.overlay];
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
        ];

        hevm = pkgs: pkgs.lib.pipe 
          (pkgs.haskellPackages.callCabal2nix "hevm" (pkgs.fetchFromGitHub {
            owner = "ethereum";
            repo = "hevm";
            rev = "d282dff6e0d9ea9f7cf02e17e8ac0b268ef634da";
            sha256 = "sha256-PQ0vm1K4DWiX6hiITdCqniMSAtxpQHezzIGdWGwmjrc=";
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
              foundry.defaultPackage.${system}
              z3
            ];
          };
        };
      }
    );
}

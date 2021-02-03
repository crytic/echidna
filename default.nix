{ pkgs ? import <nixpkgs> {} }:

let
  # fix slither, remove after this is merged https://github.com/NixOS/nixpkgs/pull/108610
  python3Packages = pkgs.python3Packages.override (oldAttrs: {
    overrides = self: super: {
        crytic-compile = super.crytic-compile.overrideAttrs (x: {
          patchPhase = ''
            substituteInPlace setup.py --replace 'version="0.1.11",' 'version="0.1.12",'
          '';
        });
        slither-analyzer = super.slither-analyzer.overrideAttrs (x: {postFixup = "";});
    };
  });

  # this is not perfect for development as it hardcodes solc to 0.5.7, test suite runs fine though
  # would be great to integrate solc-select to be more flexible, improve this in future
  solc = pkgs.stdenv.mkDerivation {
    name = "solc";
    src = if pkgs.stdenv.isDarwin then
      pkgs.fetchurl {
        url = "https://binaries.soliditylang.org/macosx-amd64/solc-macosx-amd64-v0.5.7+commit.6da8b019";
        sha256 = "0p6s5d34qz4h3463jllciiy9cgfwvp3dqlc63pa4ryjmjvi7fyil";
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

  v = "1.6.1";

  f = { mkDerivation, aeson, ansi-terminal, base, base16-bytestring
      , binary, brick, bytestring, cborg, containers, data-dword, data-has
      , deepseq, directory, exceptions, filepath, hashable, hevm, hpack
      , lens, lens-aeson, megaparsec, MonadRandom, mtl
      , optparse-applicative, process, random, stdenv, stm, tasty
      , tasty-hunit, tasty-quickcheck, temporary, text, transformers
      , unix, unliftio, unliftio-core, unordered-containers, vector
      , vector-instances, vty, wl-pprint-annotated, word8, yaml
      , cabal-install, extra, ListLike, hlint, semver
      }:
      mkDerivation rec {
        pname = "echidna";
        version = v;
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson ansi-terminal base base16-bytestring binary brick bytestring
          cborg containers data-dword data-has deepseq directory exceptions filepath
          hashable hevm lens lens-aeson megaparsec MonadRandom mtl
          optparse-applicative process random stm temporary text transformers
          unix unliftio unliftio-core unordered-containers vector
          vector-instances vty wl-pprint-annotated word8 yaml extra ListLike
          semver
        ] ++ (if pkgs.lib.inNixShell then testHaskellDepends else []);
        libraryToolDepends = [ hpack cabal-install hlint python3Packages.slither-analyzer solc ];
        executableHaskellDepends = libraryHaskellDepends;
        testHaskellDepends = [
          tasty tasty-hunit tasty-quickcheck
        ];
        preConfigure = ''
          hpack
          # re-enable dynamic build for Linux
          sed -i -e 's/os(linux)/false/' echidna.cabal
        '';
        shellHook = "hpack";
        license = stdenv.lib.licenses.agpl3;
        doHaddock = false;
        doCheck = false;
      };

  drv = pkgs.haskellPackages.callPackage f { };
in
  if pkgs.lib.inNixShell
    then drv.env
    else pkgs.haskell.lib.justStaticExecutables drv

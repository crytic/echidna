{ pkgs ? import <nixpkgs> {} }:

let
  v = "1.6.0";

  f = { mkDerivation, aeson, ansi-terminal, base, base16-bytestring
      , binary, brick, bytestring, cborg, containers, data-dword, data-has
      , deepseq, directory, exceptions, filepath, hashable, hevm, hpack
      , lens, lens-aeson, megaparsec, MonadRandom, mtl
      , optparse-applicative, process, random, stdenv, stm, tasty
      , tasty-hunit, tasty-quickcheck, temporary, text, transformers
      , unix, unliftio, unliftio-core, unordered-containers, vector
      , vector-instances, vty, wl-pprint-annotated, word8, yaml
      , cabal-install, extra, ListLike, hlint
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
        ] ++ (if pkgs.lib.inNixShell then testHaskellDepends else []);
        libraryToolDepends = [ hpack cabal-install hlint ];
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

  hevm = if pkgs.stdenv.isDarwin then
    # problems on macOS with static libs
    # hopefully will be fixed soon https://github.com/NixOS/nixpkgs/pull/105937
    let libff = pkgs.libff.overrideAttrs (attrs: {
      postPatch = pkgs.stdenv.lib.optionalString pkgs.stdenv.isDarwin ''
        substituteInPlace libff/CMakeLists.txt --replace "STATIC" "SHARED"
      '';
    });
    in pkgs.haskell.lib.addExtraLibrary pkgs.haskellPackages.hevm libff
  else
    pkgs.haskellPackages.hevm;

  drv = pkgs.haskellPackages.callPackage f { inherit hevm; };
in
  if pkgs.lib.inNixShell
    then drv.env
    else pkgs.haskell.lib.justStaticExecutables drv

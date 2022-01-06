{ pkgs ? import (builtins.fetchTarball {
    name = "nixpkgs-unstable-2021-10-15";
    url = "https://github.com/nixos/nixpkgs/archive/ee084c02040e864eeeb4cf4f8538d92f7c675671.tar.gz";
    sha256 = "sha256:1x8amcixdaw3ryyia32pb706vzhvn5whq9n8jin0qcha5qnm1fnh";
  }) {}
}:

let
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

  slither-analyzer = pkgs.slither-analyzer.override { withSolc = false; };

  v = "1.7.2";

  f = { mkDerivation, aeson, ansi-terminal, base, base16-bytestring, binary
      , brick, bytestring, cborg, containers, data-dword, data-has, deepseq
      , directory, exceptions, filepath, hashable, hevm, html-entities, hpack
      , lens, lens-aeson, megaparsec, MonadRandom, mtl, optparse-applicative
      , process, random, semver, stm, tasty, tasty-hunit, tasty-quickcheck
      , temporary, text, transformers, unix, unliftio, unliftio-core
      , unordered-containers, vector, vector-instances, vty, wl-pprint-annotated
      , word8, yaml, extra, ListLike
      }:
      mkDerivation rec {
        pname = "echidna";
        version = v;
        src = ./.;
        isLibrary = true;
        isExecutable = true;
        libraryHaskellDepends = [
          aeson ansi-terminal base base16-bytestring binary brick bytestring
          cborg containers data-dword data-has deepseq directory exceptions
          filepath hashable hevm html-entities lens lens-aeson megaparsec
          MonadRandom mtl optparse-applicative process random stm temporary
          text transformers unix unliftio unliftio-core unordered-containers
          vector vector-instances vty wl-pprint-annotated word8 yaml extra
          ListLike semver
        ] ++ (if pkgs.lib.inNixShell then testHaskellDepends else []);
        executableHaskellDepends = libraryHaskellDepends;
        testHaskellDepends = [ tasty tasty-hunit tasty-quickcheck ];
        libraryToolDepends = [ hpack ];
        testToolDepends = [ slither-analyzer solc ];
        preConfigure = ''
          hpack
          # re-enable dynamic build for Linux
          sed -i -e 's/os(linux)/false/' echidna.cabal
        '';
        shellHook = "hpack";
        license = pkgs.lib.licenses.agpl3;
        doHaddock = false;
        doCheck = true;
      };

  echidna = pkgs.haskellPackages.callPackage f { };
  echidnaShell = pkgs.haskellPackages.shellFor {
    packages = p: [ echidna ];
    buildInputs = with pkgs.haskellPackages; [
      hlint
      cabal-install
      haskell-language-server
    ];
  };
in
  if pkgs.lib.inNixShell
    then echidnaShell
    else pkgs.haskell.lib.justStaticExecutables echidna

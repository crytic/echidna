{ pkgs ? import (builtins.fetchTarball {
    name = "nixpkgs-unstable";
    url = "https://github.com/nixos/nixpkgs/archive/fb45fa64ae3460d6bd2701ab5a6c4512d781f166.tar.gz";
    sha256 = "sha256:1dcvpwshmalp5xp25z9pjm8svlnfgkcrbhany1pwjwwcpxm5kp40";
  }) {}
}:

let
  dapptools = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "dapptools";
    rev = "hevm/0.47.0";
    sha256 = "sha256-gFePasNQJ9bxrCDsSHWsZj2JVkjhVwIkjZf+wPDBKo0=";
  };
  hevm = pkgs.haskell.lib.dontCheck (
    pkgs.haskell.lib.doJailbreak (
      pkgs.haskellPackages.callCabal2nix "hevm" "${dapptools}/src/hevm"
        { secp256k1 = pkgs.secp256k1; }
  ));

  crytic-compile = pkgs.python3Packages.callPackage (import ./nix/crytic-compile.nix) { };
  # slither is shipped with solc by default, we don't use it as we need
  # precise solc versions
  slither-analyzer = pkgs.slither-analyzer.override {
    inherit crytic-compile;
    withSolc = false;
  };

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

  v = "1.7.2";

  f = { mkDerivation, aeson, ansi-terminal, base, base16-bytestring
      , binary, brick, bytestring, cborg, containers, data-dword, data-has
      , deepseq, directory, exceptions, filepath, hashable, hevm, hpack
      , lens, lens-aeson, megaparsec, MonadRandom, mtl
      , optparse-applicative, process, random, stm, tasty
      , tasty-hunit, tasty-quickcheck, temporary, text, transformers
      , unix, unliftio, unliftio-core, unordered-containers, vector
      , vector-instances, vty, wl-pprint-annotated, word8, yaml
      , cabal-install, extra, ListLike, hlint, semver, haskell-language-server
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
        libraryToolDepends = [ hpack cabal-install hlint slither-analyzer solc haskell-language-server ];
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
        license = pkgs.lib.licenses.agpl3;
        doHaddock = false;
        doCheck = true;
      };

  drv = pkgs.haskellPackages.callPackage f { hevm = hevm; };
in
  if pkgs.lib.inNixShell
    then drv.env
    else pkgs.haskell.lib.justStaticExecutables drv

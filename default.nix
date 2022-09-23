{ pkgs ? import (builtins.fetchTarball {
    name = "nixpkgs-unstable-2022-02-10";
    url = "https://github.com/nixos/nixpkgs/archive/1882c6b7368fd284ad01b0a5b5601ef136321292.tar.gz";
    sha256 = "sha256:0zg7ak2mcmwzi2kg29g4v9fvbvs0viykjsg2pwaphm1fi13s7s0i";
  }) {},
  newerPkgs ? import (builtins.fetchTarball {
    name = "nixpkgs-22.05-darwin-2022-06-27";
    url = "https://github.com/nixos/nixpkgs/archive/ce6aa13369b667ac2542593170993504932eb836.tar.gz";
    sha256 = "sha256:0d643wp3l77hv2pmg2fi7vyxn4rwy0iyr8djcw1h5x72315ck9ik";
  }) {},
  profiling ? false,
  tests ? false
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

  v = "2.0.3";

  testInputs = [ newerPkgs.slither-analyzer solc ];

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
        testToolDepends = testInputs;
        configureFlags = if profiling then [ "--enable-profiling" "--enable-library-profiling" ] else [];
        libraryToolDepends = [ hpack newerPkgs.slither-analyzer solc ];
        preConfigure = ''
          hpack
          # re-enable dynamic build for Linux
          sed -i -e 's/os(linux)/false/' echidna.cabal
        '';
        license = pkgs.lib.licenses.agpl3;
        doHaddock = false;
        doCheck = tests;
        mainProgram = "echidna-test";
      };

  dapptools = pkgs.fetchFromGitHub {
    owner = "dapphub";
    repo = "dapptools";
    rev = "hevm/0.49.0";
    sha256 = "sha256-giBcHTlFV1zJVgdbzWmezPdtPRdJQbocBEmuenBFVqk";
  };

  hevm = pkgs.haskell.lib.dontCheck (
    pkgs.haskell.lib.doJailbreak (
      pkgs.haskellPackages.callCabal2nix "hevm" "${dapptools}/src/hevm"
        { secp256k1 = pkgs.secp256k1; }
  ));

  echidna = pkgs.haskellPackages.callPackage f { hevm = hevm; };
  echidnaShell = pkgs.haskellPackages.shellFor {
    packages = p: [ echidna ];
    shellHook = "hpack";
    buildInputs = with pkgs.haskellPackages; [
      hlint
      cabal-install
    ] ++ pkgs.lib.optional (!pkgs.stdenv.isAarch64) [
      # this doesn't work due to ormolu not building
      haskell-language-server
    ];
    nativeBuildInputs = if tests then [] else testInputs;
  };
in
  if pkgs.lib.inNixShell
    then echidnaShell
    else pkgs.haskell.lib.justStaticExecutables echidna

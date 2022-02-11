{ pkgs ? import nix/pkgs.nix,
  profiling ? false,
  tests ? true
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

  v = "2.0.0";

  testInputs = [ pkgs.slither-analyzer solc ];

  f = { mkDerivation, aeson, ansi-terminal, base, base16-bytestring, binary
      , brick, bytestring, cborg, containers, data-dword, data-has, deepseq
      , directory, exceptions, filepath, hashable, hevm, hpack, lens, lens-aeson
      , megaparsec, MonadRandom, mtl, optparse-applicative, process, random
      , semver, stm, tasty, tasty-hunit, tasty-quickcheck, temporary, text
      , transformers, unix, unliftio, unliftio-core, unordered-containers, vector
      , vector-instances, vty, wl-pprint-annotated, word8, yaml, extra, ListLike
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
          filepath hashable hevm lens lens-aeson ListLike megaparsec MonadRandom
          mtl optparse-applicative process random semver stm temporary text
          transformers unix unliftio unliftio-core unordered-containers vector
          vector-instances vty wl-pprint-annotated word8 yaml extra ListLike
        ] ++ (if pkgs.lib.inNixShell then testHaskellDepends else []);
        executableHaskellDepends = libraryHaskellDepends;
        testHaskellDepends = [ tasty tasty-hunit tasty-quickcheck ];
        testToolDepends = testInputs;
        configureFlags = if profiling then [ "--enable-profiling" "--enable-library-profiling" ] else [];
        libraryToolDepends = [ hpack pkgs.slither-analyzer solc ];
        preConfigure = ''
          hpack
          # re-enable dynamic build for Linux
          sed -i -e 's/os(linux)/false/' echidna.cabal
        '';
        license = pkgs.lib.licenses.agpl3;
        doHaddock = false;
        doCheck = tests;
      };

  echidna = pkgs.haskellPackages.callPackage f { };
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

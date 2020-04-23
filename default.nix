{ nixpkgs ? import ./nix/dapptools.nix, compiler ? "default", doBenchmark ? false }:

let
  inherit (nixpkgs) pkgs;

  v = "1.5.0";

  crytic-compile = pkgs.python3Packages.callPackage (import ./nix/crytic-compile.nix) {};

  f = { mkDerivation, aeson, ansi-terminal, base, base16-bytestring
      , binary, brick, bytestring, cborg, containers, data-dword, data-has
      , deepseq, directory, exceptions, filepath, hashable, hevm, hpack
      , lens, lens-aeson, megaparsec, MonadRandom, mtl
      , optparse-applicative, process, random, stdenv, stm, tasty
      , tasty-hunit, tasty-quickcheck, temporary, text, transformers
      , unix, unliftio, unliftio-core, unordered-containers, vector
      , vector-instances, vty, wl-pprint-annotated, word8, yaml
      , cabal-install
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
          vector-instances vty wl-pprint-annotated word8 yaml
          tasty tasty-hunit tasty-quickcheck
        ] ++ (if pkgs.lib.inNixShell then testHaskellDepends else []);
        libraryToolDepends = [ hpack cabal-install ];
        executableHaskellDepends = libraryHaskellDepends;
        testHaskellDepends = [
          tasty tasty-hunit tasty-quickcheck
        ];
        executableSystemDepends = [ crytic-compile pkgs.solc-versions.solc_0_5_15 ];
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

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});
in
  if pkgs.lib.inNixShell
    then drv.env
    else pkgs.symlinkJoin {
      name = "echidna-${v}-with-deps";
      paths = [ (pkgs.haskell.lib.justStaticExecutables drv) ];
      buildInputs = [ pkgs.makeWrapper ];
      postBuild = ''
        wrapProgram $out/bin/echidna-test \
          --prefix PATH : ${pkgs.lib.makeBinPath [ crytic-compile ]} \
          --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.solc-versions.solc_0_5_15 ]}
      '';
    }

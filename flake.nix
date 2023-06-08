{
  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
    nix-bundle-exe = {
      url = "github:3noch/nix-bundle-exe";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, flake-utils, nix-bundle-exe, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
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

        hevm = pkgs.haskell.lib.dontCheck (
          pkgs.haskellPackages.callCabal2nix "hevm" (pkgs.fetchFromGitHub {
            owner = "arcz";
            repo = "hevm";
            # https://github.com/arcz/hevm/tree/forking-0.50.5
            rev = "1f1f2989e2ddc9393c2bbb42c69aeb0b80ae82ea";
            sha256 = "sha256-rlGcvVezqNrJ4okqBw1fvplijU7sFZtL9rtv2NmSrrw=";
        }) { secp256k1 = pkgs.secp256k1; });

        echidna = with pkgs; lib.pipe
          (haskellPackages.callCabal2nix "echidna" ./. { inherit hevm; })
          [
            (haskell.lib.compose.addTestToolDepends [ haskellPackages.hpack slither-analyzer solc ])
            (haskell.lib.compose.disableCabalFlag "static")
          ];
      in rec {
        packages.echidna = echidna;
        packages.default = echidna;

        packages.echidna-bundle =
          pkgs.callPackage nix-bundle-exe {} (pkgs.haskell.lib.dontCheck echidna);

        devShell = with pkgs;
          haskellPackages.shellFor {
            packages = _: [ echidna ];
            shellHook = "hpack";
            buildInputs = [
              solc
              haskellPackages.hlint
              haskellPackages.cabal-install
              haskellPackages.haskell-language-server
            ];
            withHoogle = true;
          };
      }
    );
}

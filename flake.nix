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

        patchedHaskellPackages = pkgs.haskell.packages.ghc94.override {
          overrides = self: super: {
            # disable tests in optics
            optics = pkgs.haskell.lib.dontCheck super.optics;
            # use obsidian systems fork of string-qq
            string-qq = self.callCabal2nix "string-qq" (pkgs.fetchFromGitHub {
              owner = "obsidiansystems";
              repo = "string-qq";
              rev = "82ad6d72b694dc61e9b6b7eb856cb2d3d27e2865";
              sha256 = "sha256-CNtB8jkNyNBR+ZJbtLoeA6U1ivT3gEs4UVFVHIZe27w=";
            }) {};
            # hevm needs to be jailbroken (all cabal version constraints removed)
            hevm = pkgs.haskell.lib.doJailbreak (pkgs.haskell.lib.dontCheck (
              self.callCabal2nix "hevm" (pkgs.fetchFromGitHub {
                owner = "ethereum";
                repo = "hevm";
                rev = "release/0.50.5";
                sha256 = "sha256-Vi6kL1nJdujfS1oePwqks1owVPlS5Dd5hAn0r8Rpw+k=";
              }) { secp256k1 = pkgs.secp256k1; }));
          };
        };

        echidna = with pkgs; lib.pipe
          (patchedHaskellPackages.callCabal2nix "echidna" ./. {})
          [
            (haskell.lib.compose.addTestToolDepends [ patchedHaskellPackages.hpack slither-analyzer solc ])
            (haskell.lib.compose.disableCabalFlag "static")
          ];
      in rec {
        packages.echidna = echidna;
        packages.default = echidna;

        packages.echidna-bundle =
          pkgs.callPackage nix-bundle-exe {} (pkgs.haskell.lib.dontCheck echidna);

        devShell = with pkgs;
          patchedHaskellPackages.shellFor {
            packages = _: [ echidna ];
            shellHook = "hpack";
            buildInputs = with patchedHaskellPackages; [
              solc
              hlint
              cabal-install
              haskell-language-server
            ];
            withHoogle = true;
          };
      }
    );
}

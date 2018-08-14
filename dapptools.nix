{ghc}:

let
  # Import a specific Nixpkgs revision to use as the base for our overlay.
  nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "4b649a99d8461c980e7028a693387dc48033c1f7";
    sha256 = "0iy2gllj457052wkp20baigb2bnal9nhyai0z9hvjr3x25ngck4y";
  };
in

  # Now return the Nixpkgs configured to use our overlay.
  with ( import nixpkgs ({ overlays = [(import ./dapptools/overlay.nix)]; }));

  haskell.lib.buildStackProject {
  inherit ghc;
  name = "echdina";
  buildInputs = [ hevm git bzip2 ethjet secp256k1 readline zlib ];
  }

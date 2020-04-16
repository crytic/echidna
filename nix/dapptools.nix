let
  pkgs = import <nixpkgs> {};
  src = pkgs.fetchFromGitHub {
    owner  = "dapphub";
    repo   = "dapptools";
    rev    = "af84e2ee0a0654fdaa91186384233cf1731ee7ce";
    sha256 = "091c313fw32b16mc5hjq1m388694dsqi3l3lasmddahkkp1rf1ak";
  };
in import "${src}" {}

{
  nixConfig = {
    extra-substituters = "https://trailofbits.cachix.org";
    extra-trusted-public-keys = "trailofbits.cachix.org-1:jRuxrlFghP6HstIaZg7DhvTgHyK/lcYa7U8y3CgKjzU=";
  };

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs;
    newerNixpkgs.url = github:NixOS/nixpkgs;
  };

  outputs = { self, nixpkgs, newerNixpkgs }: {
    defaultPackage.x86_64-linux = import ./. {
      pkgs = nixpkgs.legacyPackages.x86_64-linux;
      newerPkgs = newerNixpkgs.legacyPackages.x86_64-linux;
    };
    defaultPackage.aarch64-linux = import ./. {
      pkgs = nixpkgs.legacyPackages.aarch64-linux;
      newerPkgs = newerNixpkgs.legacyPackages.aarch64-linux;
    };
    defaultPackage.x86_64-darwin = import ./. {
      pkgs = nixpkgs.legacyPackages.x86_64-darwin;
      newerPkgs = newerNixpkgs.legacyPackages.x86_64-darwin;
    };
    defaultPackage.aarch64-darwin = import ./. {
      pkgs = nixpkgs.legacyPackages.aarch64-darwin;
      newerPkgs = newerNixpkgs.legacyPackages.aarch64-darwin;
    };
  };
}

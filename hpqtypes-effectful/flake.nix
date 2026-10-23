{
  description = "Development environment for hpqtypes-effectful";

  inputs = {
    # Utilitiy library for managing Nix Flakes.
    flake-utils.url = "github:numtide/flake-utils";

    # Nix package set
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
  };

  outputs = { self, flake-utils, nixpkgs }:
    flake-utils.lib.eachSystem
    (with flake-utils.lib.system; [ x86_64-linux x86_64-darwin aarch64-darwin ])
    (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
        ghc-version = "ghc8107";
        hpkgs = pkgs.haskell.packages.${ghc-version};
      in {
        devShells.default = hpkgs.shellFor {
          packages = p: [ ];
          buildInputs = with hpkgs; [
            cabal-install
            haskell-language-server
            pkgs.postgresql_13
          ];
        };
      });
}

{
  description = "A simple Scheme to AArch64 compiler.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    systems.url = "github:nix-systems/default";
  };

  outputs = {
    nixpkgs,
    systems,
    ...
  }: let
    forAllSystems = function:
      nixpkgs.lib.genAttrs (import systems) (
        system: function nixpkgs.legacyPackages.${system}
      );
  in {
    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShellNoCC {
        packages = with pkgs; [
          chez
          gcc
          just
        ];
      };
    });
  };
}

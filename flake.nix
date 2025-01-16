{
  description = "A simple Scheme to AArch64 compiler.";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

    systems.url = "github:nix-systems/default";

    pre-commit-hooks.url = "github:cachix/git-hooks.nix";
    pre-commit-hooks.inputs.nixpkgs.follows = "nixpkgs";
  };

  outputs = {
    self,
    nixpkgs,
    systems,
    pre-commit-hooks,
    ...
  }: let
    forAllSystems = function:
      nixpkgs.lib.genAttrs (import systems) (system: function nixpkgs.legacyPackages.${system});
  in {
    checks = forAllSystems (pkgs: {
      pre-commit-check = pre-commit-hooks.lib.${pkgs.stdenv.system}.run {
        src = ./.;
        hooks = {
          alejandra.enable = true;
          deadnix.enable = true;
          flake-checker.enable = true;
          nil.enable = true;
          statix.enable = true;

          typos.enable = true;
          convco.enable = true;
        };
      };
    });

    devShells = forAllSystems (pkgs: {
      default = pkgs.mkShell {
        packages = with pkgs;
          self.checks.${pkgs.stdenv.system}.pre-commit-check.enabledPackages
          ++ [
            chez
            gcc
            just
          ];
      };
    });
  };
}

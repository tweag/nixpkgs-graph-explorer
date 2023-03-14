{
  description = "nixpkgs-graph-explorer flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs_old_cypress.url = "github:NixOS/nixpkgs?rev=bf972dc380f36a3bf83db052380e55f0eaa7dcb6";
    flake-utils.url = "github:numtide/flake-utils";
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, devshell, ... }@inputs:

    flake-utils.lib.eachDefaultSystem
      (system:
        let
          pythonOverlay = self: super: {
            python = super.python310;
            python3 = self.python;
          };

          pkgs = import nixpkgs {
            inherit system;
            overlays = [ pythonOverlay inputs.devshell.overlay ];
          };
        in
        {
          devShells.default =
            pkgs.devshell.mkShell {
              motd = "nixpkgs-graph-explorer development shell{reset}\n$(type -p menu &>/dev/null && menu)\n";

              env = [];

              packages = with pkgs;[
                nodejs-19_x
               poetry
               python
              ];
              commands = [];
            };
        });
}
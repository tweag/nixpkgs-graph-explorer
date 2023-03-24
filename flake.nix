{
  description = "nixpkgs-graph-explorer flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
    nixpkgs_old_cypress.url =
      "github:NixOS/nixpkgs?rev=bf972dc380f36a3bf83db052380e55f0eaa7dcb6";
    flake-utils.url = "github:numtide/flake-utils";
    devshell = {
      url = "github:numtide/devshell";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, devshell, ... }@inputs:

    flake-utils.lib.eachDefaultSystem (system:
      let
        pythonOverlay = self: super: {
          python = super.python310;
          python3 = self.python;
        };

        pkgs = import nixpkgs {
          inherit system;
          overlays = [ pythonOverlay inputs.devshell.overlay ];
        };

      in {
        devShells.default = pkgs.devshell.mkShell {
          motd = ''
            nixpkgs-graph-explorer development shell{reset}
            $(type -p menu &>/dev/null && menu)
          '';

          env = [ ];

          packages = with pkgs; [ nodejs-19_x poetry python nixfmt ];
          commands = [
            ##################################################################
            # Typescript commands
            ##################################################################
            # TODO: Add these
            ##################################################################
            # Python-specific commands
            ##################################################################
            {
              name = "format-python";
              help = "Format Python code";
              command = ''
                echo 'Formatting code with black...'
                poetry run black .
                echo 'Formatting imports with isort...'
                poetry run isort .
              '';
              category = "Python";
            }
            {
              name = "check-python";
              help = "Check Python formatting";
              command = ''
                poetry run black --check .
                poetry run isort --check-only .
              '';
               category = "Python";
            }
            {
              name = "lint-python";
              help = "Lint Python code";
              command = ''
                poetry run flake8 .
                poetry run pyright .
              '';
               category = "Python";
            }
            ##################################################################
            # Project-wide commands
            ##################################################################
            {
              name = "format";
              help = "Format code for entire project";
              command = ''
                format-python
              '';
            }
            {
              name = "check";
              help = "Check formatting for entire project";
              command = ''
                check-python
              '';
            }
            {
              name = "lint";
              help = "Lint entire project";
              command = ''
                lint-python
              '';
            }
          ];
        };
      });
}

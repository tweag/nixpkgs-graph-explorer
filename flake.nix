{
  description = "nixpkgs-graph-explorer flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
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
      in
      {
        devShells.default = pkgs.devshell.mkShell {
          motd = ''
            nixpkgs-graph-explorer development shell{reset}
            $(type -p menu &>/dev/null && menu)
          '';

          env = [ ];

          packages = with pkgs; [
            nodejs-19_x
            nodePackages.prettier

            poetry
            python

            nixpkgs-fmt
          ];
          commands =
            let
              prettierCmd =
                ''
                  prettier \
                    --prose-wrap always \
                    --loglevel warn \
                    --ignore-path .gitignore \
                '';
            in
            [
              ##################################################################
              # Frontend commands
              ##################################################################
              {
                name = "format-prettier";
                help = "Format frontend code";
                command = ''
                  echo 'Formatting code with prettier...'
                  ${prettierCmd} --write web
                '';
                category = "Frontend";
              }
              {
                name = "check-prettier";
                help = "Check Prettier formatting";
                command = ''
                  ${prettierCmd} --check web
                '';
                category = "Frontend";
              }

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
              # Nix-specific commands
              ##################################################################
              {
                name = "format-nix";
                help = "Format Nix code";
                command = ''
                  echo 'Formatting code with nixpkgs-fmt...'
                  nixpkgs-fmt .
                '';
                category = "Nix";
              }
              {
                name = "check-nix";
                help = "Check Nix formatting";
                command = ''
                  nixpkgs-fmt --check .
                '';
                category = "Nix";
              }
              ##################################################################
              # Project-wide commands
              ##################################################################
              {
                name = "format";
                help = "Format code for entire project";
                command = ''
                  format-python
                  format-prettier
                  format-nix
                '';
              }
              {
                name = "check";
                help = "Check formatting for entire project";
                command = ''
                  check-python
                  check-prettier
                  check-nix
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

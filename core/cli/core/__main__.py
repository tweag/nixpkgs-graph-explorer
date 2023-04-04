import click
import subprocess
import pathlib


@click.command()
def extract_data():
    nixpkgs_graph_nix_file_path = (
        pathlib.Path(__file__).parent.joinpath("nixpkgs-graph.nix").resolve()
    )
    result = subprocess.run(
        'NIXPKGS_ALLOW_BROKEN=1 NIXPKGS_ALLOW_INSECURE=1 TARGET_FLAKE_REF="github:rapenne-s/bento" TARGET_SYSTEM="x86_64-linux" nix eval --json --file '
        + str(nixpkgs_graph_nix_file_path)
        + ' --extra-experimental-features "nix-command flakes"',
        shell=True,
        check=True,
        stdout=subprocess.PIPE,
    )
    print(result)


if __name__ == "__main__":
    extract_data()

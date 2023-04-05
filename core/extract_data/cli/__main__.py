import click
import subprocess
import pathlib


@click.command()
@click.option(
    "--target-flake-ref",
    default="github:nixos/nixpkgs/master",
    help="The reference of the target Nix Flake",
)
@click.option(
    "--target-system",
    default="x86_64-linux",
    help="The system in which to evaluate the packages",
)
@click.option(
    "--out",
    default="nodes.json",
    help="The output path at which to write the data in JSON format",
)
@click.option(
    "--stdout",
    default=False,
    help="Write outputs to stdout instead of to a file",
    is_flag=True,
)
def extract_data(target_flake_ref: str, target_system: str, out: str, stdout: bool):
    nixpkgs_graph_nix_file_path = (
        pathlib.Path(__file__).parent.joinpath("nixpkgs-graph.nix").resolve()
    )
    result = subprocess.run(
        'NIXPKGS_ALLOW_BROKEN=1 NIXPKGS_ALLOW_INSECURE=1 TARGET_FLAKE_REF="'
        + target_flake_ref
        + '" TARGET_SYSTEM="'
        + target_system
        + '" nix eval --json --file '
        + str(nixpkgs_graph_nix_file_path)
        + ' --extra-experimental-features "nix-command flakes"',
        shell=True,
        check=True,
        stdout=subprocess.PIPE,
    ).stdout.decode()
    if stdout:
        print(result)
    else:
        click.echo(f"Writing data to {out}")
        with open(out, "w") as f:
            f.write(result)
        click.echo(f"Done.")


if __name__ == "__main__":
    extract_data()

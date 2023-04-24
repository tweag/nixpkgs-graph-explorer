import logging
import os
import pathlib
import subprocess
from typing import IO, Any

import click


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
    "--verbose",
    "-v",
    is_flag=True,
    help="Increase verbosity",
)
@click.argument(
    "outfile",
    type=click.File("wb"),
)
def extract_data(
    target_flake_ref: str, target_system: str, verbose: bool, outfile: IO[Any]
):
    """
    Extract the graph of derivations from a flake as JSON.

    OUTFILE is the path to the output file to write to, use "-" to write to stdout.
    """
    logger = logging.getLogger(__name__)
    if verbose:
        logger.setLevel(logging.INFO)

    # Nix expression to evaluate with Nix
    nixpkgs_graph_nix_file_path = (
        pathlib.Path(__file__).parent.joinpath("nixpkgs-graph.nix").resolve()
    )

    # overwrite some environment variables
    extra_env = {
        # required to evaluate some Nixpkgs expressions
        "NIXPKGS_ALLOW_BROKEN": "1",
        "NIXPKGS_ALLOW_INSECURE": "1",
        # arguments to the Nix expression can't be passed with `nix eval`
        # we use environment variables instead
        "TARGET_FLAKE_REF": str(target_flake_ref),
        "TARGET_SYSTEM": str(target_system),
    }
    logger.info(
        "extra_env=%s",
        " ".join(f"{k}={v}" for k, v in extra_env.items()),
    )
    env = os.environ.copy()
    for k, v in extra_env.items():
        env[k] = v

    cmd = [
        "nix",
        "eval",
        "--extra-experimental-features",
        "nix-command flakes",
        "--json",
        "--file",
        str(nixpkgs_graph_nix_file_path),
    ]
    logger.info("cmd=%s", " ".join(cmd))
    subprocess.run(
        cmd,
        # raise an exception on failure
        check=True,
        # write to file passed as argument (or stdout)
        stdout=outfile,
        env=env,
    )


if __name__ == "__main__":
    logging.basicConfig()
    extract_data()

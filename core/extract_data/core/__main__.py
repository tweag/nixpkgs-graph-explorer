import pathlib
import subprocess

from pydantic import ValidationError

from explorer.core.model import NixGraph


def extract_data(target_flake_ref: str, target_system: str, out: str):
    # Extract data from Nix Flake
    nixpkgs_graph_nix_file_path = (
        pathlib.Path(__file__).parent.joinpath("nixpkgs-graph.nix").resolve()
    )
    result = subprocess.run(
        # We add ALLOW_UNSUPPORTED_SYSTEM=1 here because for Package
        # ‘apple-framework-CoreServices’in not available on the requested hostPlatform
        "NIXPKGS_ALLOW_UNSUPPORTED_SYSTEM=1 NIXPKGS_ALLOW_BROKEN=1"
        + 'NIXPKGS_ALLOW_INSECURE=1 TARGET_FLAKE_REF="'
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

    # Writing data to file
    print(f"Writing data from {target_flake_ref} to {out}...")
    with open(out, "w") as f:
        f.write(result)

    # Validate data
    print("Validating data...")
    try:
        NixGraph.parse_raw(result)
        print("The data has been validated against the NixGraph schema with no issues.")
    except ValidationError as e:
        print(e)


if __name__ == "__main__":
    extract_data(
        target_flake_ref="github:nixos/nixpkgs/master",
        target_system="x86_64-linux",
        out="nodes.json",
    )

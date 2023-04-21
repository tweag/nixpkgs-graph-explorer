import pathlib
import subprocess
from typing import List, Optional, Set

import pandas as pd

from explorer.core.model import BuildInput, NixGraph, NixpkgsMetadata, Package


def extract_from_nix() -> pd.DataFrame:
    nixpkgs_graph_nix_file_path = (
        pathlib.Path(__file__).parent.parent.joinpath("nixpkgs-graph.nix").resolve()
    )
    result = subprocess.run(
        "NIXPKGS_ALLOW_BROKEN=1 NIXPKGS_ALLOW_INSECURE=1"
        + ' TARGET_FLAKE_REF="github:nixos/nixpkgs/master" TARGET_SYSTEM="x86_64-linux"'
        + " nix eval --json --file "
        + str(nixpkgs_graph_nix_file_path)
        + ' --extra-experimental-features "nix-command flakes"',
        shell=True,
        check=True,
        stdout=subprocess.PIPE,
    ).stdout.decode()
    dataframe = pd.read_json(result, orient="records")
    return dataframe


def add_build_inputs(nix_graph: NixGraph, dataframe: pd.DataFrame) -> NixGraph:
    """
    Converts a pandas DataFrame containing Nix package data into a NixGraph object.

    Args:
        dataframe(pd.DataFrame): The dataframe containing Nix package data.

    Returns:
        NixGraph: A NixGraph object representing the packages and their dependencies.
    """
    print("Converting the Nix package data into a NixGraph object...\n")
    # packages = nix_graph.packages
    # Add Package object to a list with an empty `build_inputs` and other properties.
    for i in range(len(dataframe)):
        for input_ in dataframe.packages[i]["buildInputs"]:
            for j in range(len(dataframe)):
                if input_ == dataframe.packages[j]["drvPath"]:
                    nix_graph.packages[i].build_inputs.append(
                        BuildInput(
                            build_input_type="build_input",
                            package=nix_graph.packages[j],
                        )
                    )
                    break

        for input_ in dataframe.packages[i]["propagatedBuildInputs"]:
            for j in range(len(dataframe)):
                if input_ == dataframe.packages[j]["drvPath"]:
                    nix_graph.packages[i].build_inputs.append(
                        BuildInput(
                            build_input_type="propagated_build_input",
                            package=nix_graph.packages[j],
                        )
                    )
                    break
        print("Adding build inputs for the " + str(i) + "th package", end="\r")
    return nix_graph

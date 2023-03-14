import subprocess
import pandas as pd
import pathlib
from pprint import pprint
from contextlib import closing
from typing import List, Set, Optional

from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.graph_traversal import __


def extract_from_nix() -> pd.DataFrame:
    nixpkgs_graph_nix_file_path = (
        pathlib.Path(__file__).parent.joinpath("nixpkgs-graph.nix").resolve()
    )
    print("Downloading data from nix as a dataframe...")
    result = subprocess.run(
        'NIXPKGS_FLAKE_REF="github:nixos/nixpkgs/master" nix eval --json --file '
        + str(nixpkgs_graph_nix_file_path)
        + ' --extra-experimental-features "nix-command flakes"',
        shell=True,
        check=True,
        stdout=subprocess.PIPE,
    )
    dataframe = pd.read_json(result.stdout.decode(), orient="records")
    return dataframe


def process_data(dataframe: pd.DataFrame) -> pd.DataFrame:
    dataframe["path"] = dataframe.groupby(["outputPath"])["path"].transform(
        lambda x: ", ".join(x)
    )
    dataframe = dataframe.loc[dataframe.astype(str).drop_duplicates().index]
    dataframe["path"] = dataframe["path"].fillna("")
    dataframe = dataframe.reset_index(drop=True)
    output_names = get_output_names(dataframe)
    dataframe["buildInputsName"] = dataframe["buildInputs"].apply(
        (lambda x: path_to_name(x, output_names))
    )
    dataframe["propagatedBuildInputsName"] = dataframe["propagatedBuildInputs"].apply(
        (lambda x: path_to_name(x, output_names))
    )
    return dataframe


def get_output_names(dataframe: pd.DataFrame) -> Set[str]:
    """Returns a set of all possible path names of outputPath of the dataframe."""
    return set(ele["name"] for paths in dataframe["outputPathAll"] for ele in paths)


def path_to_name(x: list, output_names: Set[str]) -> List[str]:
    """Takes in a list of full addresses, extracts the package name from each of the address, and returns a list of package names."""
    names = []
    for p in x:
        if p != None:
            names.append(
                "-".join(p.split("-")[1:-1])
                if p.split("-")[-1] in output_names
                else p[44:]
            )
    return names


def path_to_outputpath(dataframe: pd.DataFrame, path: str, name: str) -> Optional[str]:
    """Analyzes the full address of a package, and maps it to the output path with the same package.

    Args:
        dataframe(pd.DataFrame): The dataframe consisting of name, outputPath, outputPathAll, and other attributes.
        path(str): The full address of a package.
        name(str): The package name of which the full address 'path' refers to.
    """
    df_name = dataframe.query("name == @name")
    mask = df_name.outputPathAll.apply(lambda x: path in [ele["path"] for ele in x])
    if df_name[mask].empty:
        return None
    return df_name[mask].outputPath.iloc[0]


def unique_insert_node(g, row):
    g.V().has("package", "outputPath", str(row["outputPath"])).fold().coalesce(
        __.unfold(),
        __.addV("package")
        .property("outputPath", str(row["outputPath"]))
        .property("path", row["path"])
        .property("pname", row["pname"])
        .property("name", row["name"])
        .property("version", row["version"])
        .property("brokenState", str(row["brokenState"]))
        .property("license", row["license"]),
    ).iterate()


def unique_insert_edge(g, row, target, label):
    if g.V().has("package", "outputPath", target).toList() != []:
        g.V().has("package", "outputPath", row["outputPath"]).as_("v").V().has(
            "package", "outputPath", target
        ).coalesce(
            __.inE("dependsOn").where(__.outV().as_("v")),
            __.addE("dependsOn").from_("v").property("label", label),
        ).iterate()


def ingest_graph(dataframe: pd.DataFrame, connstring: str) -> None:
    """Ingests a Pandas DataFrame containing package properties and dependency information and writes the resulting graph to a Gremlin server.

    The function removes any existing nodes and edges from the graph, adds the nodes and edges defined in the DataFrame, and then queries the number of nodes and edges in the graph.

    Args:
        dataframe (pd.DataFrame): A Pandas DataFrame containing package properties and dependency information. The DataFrame should have the following columns:
            - outputPath (str): the output path of the package
            - outputPathAll (list[dict]): list of dicts containing output names and their output paths
            - path (str): the path of the package
            - pname (str): the pname of the package
            - name (str): the name of the package
            - version (str): the version of the package
            - brokenState (bool): indicates if the package is in a broken state
            - license (str): the license of the package
            - buildInputs (list[str]): list of build inputs of the package
            - propagatedBuildInputs (list[str]): list of propagated build inputs of the package
            - buildInputsName (list[str]): list of names of the build inputs
            - propagatedBuildInputsName (list[str]): list of names of the propagated build inputs
        connstring (str): A connection string for connecting to the Gremlin server

    Returns:
        None. The function writes the graph to a Gremlin server and prints status messages to the console.
    """
    with closing(DriverRemoteConnection(connstring, "g")) as remote:
        g = traversal().withRemote(remote)
        # Remove nodes and edges
        print("Initiating... (removing nodes and edges)")
        g.V().drop().iterate()
        g.E().drop().iterate()

        # Add nodes
        print("Adding nodes...")
        for _, row in dataframe.iterrows():
            # TODO: there's some outputPath = None
            unique_insert_node(g, row)
            print(str(_) + " node(s) added", end="\r")
        print("\n")

        # Add edges
        print("Adding edges...")
        for _, row in dataframe.iterrows():
            if row["outputPath"] != None:
                for i in range(len(row["buildInputs"])):
                    target_outputpath = path_to_outputpath(
                        dataframe, row["buildInputs"][i], row["buildInputsName"][i]
                    )
                    if target_outputpath is not None:
                        unique_insert_edge(g, row, target_outputpath, "buildInputs")
                for i in range(len(row["propagatedBuildInputs"])):
                    target_outputpath = path_to_outputpath(
                        dataframe,
                        row["propagatedBuildInputs"][i],
                        row["propagatedBuildInputsName"][i],
                    )
                    if target_outputpath is not None:
                        unique_insert_edge(
                            g, row, target_outputpath, "propagatedBuildInputs"
                        )
            print("Adding edges started from the " + str(_) + "(th) node", end="\r")
        print("\n")

        # Query number of nodes
        print("Querying number of nodes...")
        pprint(g.V().count().toList())

        # Query number of edges
        print("Querying number of edges...")
        pprint(g.E().count().toList())

        print("Done.")


def main():
    # Extract data from nix as a dataframe
    df = extract_from_nix()
    # Process data by grouping by outputPath and concatenating `path` as a single string
    df = process_data(df)
    # Load data to database via sqlg and query the data
    ingest_graph(df, "ws://localhost:8182/gremlin")


if __name__ == "__main__":
    main()

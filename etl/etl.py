import subprocess
import pandas as pd
import pathlib
from pprint import pprint
from contextlib import closing

from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.graph_traversal import __

def extract_from_nix():    
    nixpkgs_graph_nix_file_path = (
        pathlib.Path(__file__).parent.joinpath("nixpkgs-graph.nix").resolve()
    )
    print("Downloading data from nix as a dataframe...")    
    result = subprocess.run('NIXPKGS_FLAKE_REF="github:nixos/nixpkgs/master" nix eval --json --file '+ str(nixpkgs_graph_nix_file_path) + ' --extra-experimental-features "nix-command flakes"', shell=True, stdout=subprocess.PIPE)
    dataframe = pd.read_json(result.stdout.decode(), orient="records")
    return dataframe

def data_process(dataframe):
    dataframe["path"] = dataframe.groupby(["outputPath"])["path"] \
                                 .transform(lambda x : ", ".join(x))
    dataframe = dataframe.loc[dataframe.astype(str).drop_duplicates().index]
    dataframe["path"] = dataframe["path"].fillna("")
    dataframe = dataframe.reset_index(drop=True)
    outputName = get_outputNames(dataframe)
    dataframe["buildInputsName"] = \
        dataframe["buildInputs"].apply((lambda x : \
                                        path_to_name(x, outputName)))
    dataframe["propagatedBuildInputsName"] = \
        dataframe["propagatedBuildInputs"].apply((lambda x : \
                                                  path_to_name(x, outputName)))
    return dataframe

def get_output_names(dataframe):
    return set(
        name
         for names in dataframe["outputNameAll"].drop_duplicates()
         for name in names
     )

def path_to_name(x: list, outputName):
    names = []
    for p in x:
        if p != None:
            names.append("-".join(p.split("-")[1:-1]) \
                         if p.split("-")[-1] in outputName \
                         else p[44:] )
    return names

def path_to_outputpath(dataframe: pd.DataFrame, path: str, name: str) -> Optional[str]:
    df_name = dataframe.query('name == @name')
    if df_name.empty:
        return None
    for _, row in df_name.iterrows():
        if path in row["outputPathAll"]:
            return row["outputPath"]
    else:
        return None

def unique_insert_node(g, row):
    g.V().has("package", "outputPath", str(row["outputPath"])) \
        .fold() \
        .coalesce(__.unfold(), \
                  __.addV("package") \
                  .property("outputPath", str(row["outputPath"])) \
                  .property("path", row["path"]) \
                  .property("pname", row["pname"]) \
                  .property("name", row["name"]) \
                  .property("version", row["version"])) \
        .iterate()

def unique_insert_edge(g, row, target, label):
    if g.V().has("package", "outputPath", target).toList() != []:
        g.V().has("package", "outputPath", row["outputPath"]) \
             .as_("v") \
             .V().has("package", "outputPath", target) \
                 .coalesce(__.inE("dependsOn") \
                           .where(__.outV().as_("v")), \
                           __.addE("dependsOn").from_("v") \
                           .property("label", label)) \
                 .iterate()

def gremlin_queries(dataframe):
    with closing(DriverRemoteConnection('ws://localhost:8182/gremlin', "g")) as remote:
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
            print(str(_) + " node(s) added", end='\r')
        print('\n')

        # Add edges
        print("Adding edges...")
        for _, row in dataframe.iterrows():
            if row["outputPath"] != None:
                for i in range(len(row["buildInputs"])):
                    target_outputpath = path_to_outputpath(dataframe, \
                                                           row["buildInputs"][i], \
                                                           row["buildInputsName"][i])
                    if target_outputpath is not None:
                        unique_insert_edge(g, row, target_outputpath, "buildInputs")
                for i in range(len(row["propagatedBuildInputs"])):
                    target_outputpath = path_to_outputpath(dataframe, \
                                                           row["propagatedBuildInputs"][i], \
                                                           row["propagatedBuildInputsName"][i])
                    if target_outputpath is not None:
                        unique_insert_edge(g, row, target_outputpath, "propagatedBuildInputs")
            print("Adding edges started from the " + str(_) + "(th) node", end='\r')
        print('\n')

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
    df = data_process(df)
    # Load data to database via sqlg and query the data
    gremlin_queries(df)

if __name__ == "__main__":
    main()

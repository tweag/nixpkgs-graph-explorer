import subprocess
import pandas as pd
import requests
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

def unique_insert_node(g, row):
    g.V().has("outputPath", str(row["outputPath"])) \
        .fold() \
        .coalesce(__.unfold(), \
                  __.addV() \
                  .property("outputPath", str(row["outputPath"])) \
                  .property("path", row["path"]) \
                  .property("pname", row["pname"]) \
                  .property("version", row["version"])) \
        .iterate()

def unique_insert_edge(g, row, target):
    if g.V().has("outputPath", target).toList() != []:
        g.V().has("outputPath", row["outputPath"]) \
             .as_("v") \
             .V().has("outputPath", target) \
                 .coalesce(__.inE("buildInputs") \
                           .where(__.outV().as_("v")), \
                           __.addE('buildInputs').from_("v") \
                           .property("label", "buildInputs")) \
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
    
        # Add edges
        print("Adding edges...")
        for _, row in dataframe.iterrows():
            if row["outputPath"] != None:
                for target in row["buildInputs"]:
                    unique_insert_edge(g, row, target)
            print("Adding edges started from the " + str(_) + "(th) node", end='\r')

        print("Querying number of nodes...")
        pprint(g.V().count().toList())

        print("Querying number of edges...")
        pprint(g.E().count().toList())
        print("Done.")
        
def main():
    # Extract data from nix as a dataframe
    df = extract_from_nix()
    # Load data to database via sqlg and query the data
    gremlin_queries(df)

if __name__ == "__main__":
    main()

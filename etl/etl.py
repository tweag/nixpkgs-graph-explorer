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

def gremlin_queries(dataframe):
    # have a small subset of dataframe
    dataframe = dataframe[:1000]
    with closing(DriverRemoteConnection('ws://localhost:8182/gremlin', "g")) as remote:
        g = traversal().withRemote(remote)
        # Remove nodes and edges
        g.V().drop().iterate()
        g.E().drop().iterate()
    
        # Add nodes
        print("Adding nodes...")
        for _, row in dataframe.iterrows():
            # TODO: there's some outputPath = None
            g.addV("outputPath").property("outputPath", str(row["outputPath"])).iterate()
            print(str(_) + " node(s) added", end='\r')
    
        # Add edges
        print("Adding edges...")
        for _, row in dataframe.iterrows():
            if row["outputPath"] != None:
                for target in row["buildInputs"]:
                    if g.V().has("outputPath", target).toList() != []:
                        g.V().has("outputPath", row["outputPath"]).addE("buildInputs").to(__.V().has("outputPath", target)).property("label", "buildInputs").iterate()

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

from pprint import pprint
from contextlib import closing

from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.graph_traversal import __
from gremlin_python.process.strategies import *

def main():
    with closing(DriverRemoteConnection('ws://localhost:8182/gremlin', 'gReadOnly')) as remote:
        g = traversal().withRemote(remote)

        print("Querying an example package...")
        pprint(g.V() \
               .has("package", "outputPath", '/nix/store/gqidkrgxxfkgli0zsl2aj492zp3cp6si-qt3d-5.12.10') \
               .elementMap() \
               .toList())
        
        print("Testing if the database is read-only...")
        try:
            g.addV("shouldFail").iterate()
        except:
            print("Failed adding nodes.")

        print("Done.")
            

if __name__ == "__main__":
    main()

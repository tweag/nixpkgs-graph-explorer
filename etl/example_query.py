from pprint import pprint
from contextlib import closing

from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection

def main():
    print("WARNING: this is a WIP, this script is a proof of concept")
    with closing(DriverRemoteConnection('ws://localhost:8182/gremlin', 'g')) as remote:
        g = traversal().withRemote(remote)

        print("Querying existing verticies...")
        pprint(g.V().elementMap().limit(10).toList())
        print("Adding an example vertex...")
        pprint(g.addV("fooLabel").toList())
        print("Querying verticies again...")
        pprint(g.V().elementMap().limit(10).toList())
        print("Removing verticies...")
        pprint(g.V().drop().iterate())
        print("Done.")

if __name__ == "__main__":
    main()

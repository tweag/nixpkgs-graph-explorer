from pprint import pprint
from contextlib import contextmanager

from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection

@contextmanager
def gremlin_remote_connection(url: str):
    r = DriverRemoteConnection(url,"g")
    yield r
    r.close()

def main():
    with gremlin_remote_connection('ws://localhost:8182/gremlin') as remote:
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

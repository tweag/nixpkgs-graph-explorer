from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
import pytest
from gremlin_python.driver import serializer
import explorer.queries.packages as packages

from explorer.queries.pagination import Cursor, CursorDirection
from gremlin_python.process.anonymous_traversal import traversal
from explorer.graph import insert_unique_vertex, Package


DUMMY_PACKAGES = [
    Package("package-a", "a/a/a"),
    Package("package-b", "b/b/b"),
    Package("package-c", "c/c/c"),
    Package("package-d", "c/c/c"),
]


@pytest.fixture
def graph_connection():
    conn = DriverRemoteConnection(
        "ws://localhost:8182/gremlin",
        "g",
        message_serializer=serializer.GraphSONMessageSerializer(),
    )
    g = traversal().withRemote(conn)
    # Insert test dataset
    for p in DUMMY_PACKAGES:
        insert_unique_vertex(p, g)
    yield conn
    # Dropping all data from graph on teardown
    g.V().drop().iterate()
    g.E().drop().iterate()
    conn.close()


# FIXME: Add additional test cases
#
# - Page size exceeds available data
# - Forward cursor direction
# - Previous cursor direction
# - No cursor
# - Search with cursor
# - Search with no cursor


# @pytest.mark.db_integration
def test_packages_query(graph_connection):
    # Note: This test was just for hacking around
    cursor = None
    request = packages.ListPackagesRequest(cursor, None)
    response = packages.list_packages(request, graph_connection)
    print(response)
    assert True

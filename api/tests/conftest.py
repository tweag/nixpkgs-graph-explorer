import pytest
from gremlin_python.process.anonymous_traversal import traversal

from explorer.api.graph import insert_unique_directed_edge, insert_unique_vertex
from explorer.api.gremlin import default_remote_connection
from tests.constants import DUMMY_DERIVATIONS, EDGES


@pytest.fixture
def empty_graph_connection():
    # FIXME: use a traversal_source purely dedicated to tests
    conn = default_remote_connection(
        "ws://localhost:8182/gremlin", traversal_source="g"
    )
    g = traversal().with_remote(conn)
    # Make sure we only use the test data
    g.V().drop().iterate()
    g.E().drop().iterate()
    yield conn
    # Dropping all data from graph on teardown
    g.V().drop().iterate()
    g.E().drop().iterate()
    conn.close()


@pytest.fixture
def populated_graph_connection(empty_graph_connection):
    g = traversal().with_remote(empty_graph_connection)
    graph_traversal = g.get_graph_traversal()

    # Add nodes
    for drv in DUMMY_DERIVATIONS:
        graph_traversal = insert_unique_vertex(graph_traversal, drv)
    graph_traversal.iterate()

    # Add edges between them
    for edge, from_vertex, to_vertex in EDGES:
        insert_unique_directed_edge(g, edge, from_vertex, to_vertex)
    yield empty_graph_connection

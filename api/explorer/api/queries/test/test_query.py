import pytest
from gremlin_python.driver import serializer
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.anonymous_traversal import traversal

from explorer.api.graph import (
    DependsOn,
    Package,
    insert_unique_directed_edge,
    insert_unique_vertex,
)
from explorer.api.queries.cytoscape import (
    CytoscapeJs,
    EdgeData,
    EdgeDefinition,
    ElementsDefinition,
    NodeData,
    NodeDefinition,
)

PACKAGE_A = Package(pname="package-a", outputPath="a/a/a")
PACKAGE_B = Package(pname="package-b", outputPath="b/b/b")
PACKAGE_C = Package(pname="package-c", outputPath="c/c/c")
PACKAGE_AA = Package(pname="package-aa", outputPath="aa/aa/aa")

DUMMY_PACKAGES = [PACKAGE_A, PACKAGE_B, PACKAGE_C, PACKAGE_AA]

EDGES = [
    (DependsOn(), PACKAGE_A, PACKAGE_B),
    (DependsOn(), PACKAGE_B, PACKAGE_C),
    (DependsOn(), PACKAGE_B, PACKAGE_AA),
]


@pytest.fixture
def graph_connection():
    conn = DriverRemoteConnection(
        "ws://localhost:8182/gremlin",
        "g",
        message_serializer=serializer.GraphSONMessageSerializer(),
    )
    g = traversal().with_remote(conn)
    # Insert package vertices
    for p in DUMMY_PACKAGES:
        insert_unique_vertex(p, g)
    # Add edges between them
    for edge, from_vertex, to_vertex in EDGES:
        insert_unique_directed_edge(edge, from_vertex, to_vertex, g)
    yield conn
    # Dropping all data from graph on teardown
    g.V().drop().iterate()
    g.E().drop().iterate()
    conn.close()


def test_gremlin_query_non_path_end_step(graph_connection: DriverRemoteConnection):
    from explorer.api.queries.query import GremlinResult, QueryResult, WarningMessage

    # Execute a query
    query = "g.V().count()"
    result = GremlinResult(graph_connection._client, query).to_query_result()
    expected = QueryResult(
        raw="[4]", warning=WarningMessage.RESULT_IS_NOT_PLOTTABLE, cyto=None
    )
    assert result == expected


def test_gremlin_query_path_step(graph_connection: DriverRemoteConnection):
    from explorer.api.queries.query import GremlinResult, TableEntry

    # Execute a query which returns a path with required 'pname' and 'label' properties
    query = f"""
        g.V()
         .has(
            '{PACKAGE_A.label()}',
            '{PACKAGE_A.id_property_name()}',
            '{PACKAGE_A.get_id()}'
         )
        .repeat(outE().otherV().simplePath())
        .until(
            outE().count().is(0)
        )
        .path()
        .by('pname')
        .by('label')
    """
    result = GremlinResult(graph_connection._client, query).to_query_result()
    expected_table_data = [
        TableEntry(id=PACKAGE_A.pname, neighbours=[PACKAGE_B.pname]),
        TableEntry(id=PACKAGE_B.pname, neighbours=[PACKAGE_C.pname, PACKAGE_AA.pname]),
        TableEntry(id=PACKAGE_C.pname, neighbours=[]),
        TableEntry(id=PACKAGE_AA.pname, neighbours=[]),
    ]
    expected_edges = [
        EdgeDefinition(data=EdgeData(source=PACKAGE_A.pname, target=PACKAGE_B.pname)),
        EdgeDefinition(data=EdgeData(source=PACKAGE_B.pname, target=PACKAGE_C.pname)),
        EdgeDefinition(data=EdgeData(source=PACKAGE_B.pname, target=PACKAGE_AA.pname)),
    ]
    expected_nodes = [
        NodeDefinition(data=NodeData(id=PACKAGE_A.pname)),
        NodeDefinition(data=NodeData(id=PACKAGE_B.pname)),
        NodeDefinition(data=NodeData(id=PACKAGE_C.pname)),
        NodeDefinition(data=NodeData(id=PACKAGE_AA.pname)),
    ]
    expected_graph_data = CytoscapeJs(
        elements=ElementsDefinition(nodes=expected_nodes, edges=expected_edges)
    )

    assert result.cyto is not None
    assert result.cyto.table_data == expected_table_data
    assert result.cyto.graph_data == expected_graph_data
    # Note: Not including an assertion for `raw` property since that value is hard
    # to construct manually for a more complex query such as the one in this test.

import pytest
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.anonymous_traversal import traversal

from explorer.api.graph import (
    HasBuildInput,
    HasPropagatedBuildInput,
    Derivation,
    insert_unique_directed_edge,
    insert_unique_vertex,
)
from explorer.api.gremlin import default_remote_connection
from explorer.api.queries.cytoscape import (
    CytoscapeJs,
    EdgeData,
    EdgeDefinition,
    ElementsDefinition,
    NodeData,
    NodeDefinition,
)

DERIVATION_A = Derivation(output_path="/nix/store/a", attribute_path="a")
DERIVATION_B = Derivation(output_path="/nix/store/b", attribute_path="b")
DERIVATION_C = Derivation(output_path="/nix/store/c", attribute_path="c")
DERIVATION_AA = Derivation(output_path="/nix/store/aa", attribute_path="a.a")

DUMMY_DERIVATIONS = [DERIVATION_A, DERIVATION_B, DERIVATION_C, DERIVATION_AA]

EDGES = [
    (HasBuildInput(), DERIVATION_A, DERIVATION_B),
    (HasBuildInput(), DERIVATION_B, DERIVATION_C),
    (HasPropagatedBuildInput(), DERIVATION_B, DERIVATION_AA),
]


@pytest.fixture
def graph_connection():
    # FIXME: use a traversal_source purely dedicated to tests
    conn = default_remote_connection(
        "ws://localhost:8182/gremlin", traversal_source="g"
    )
    g = traversal().with_remote(conn)
    # Insert derivation vertices
    for p in DUMMY_DERIVATIONS:
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

    # Execute a query which returns a path with required 'output_path' and 'label'
    # properties
    query = f"""
        g.V()
         .has(
            '{DERIVATION_A.label()}',
            '{DERIVATION_A.id_property_name()}',
            '{DERIVATION_A.get_id()}'
         )
        .repeat(outE().otherV().simplePath())
        .until(
            outE().count().is(0)
        )
        .path()
        .by('output_path')
        .by('label')
    """
    result = GremlinResult(graph_connection._client, query).to_query_result()
    expected_table_data = [
        TableEntry(id=DERIVATION_A.output_path, neighbours=[DERIVATION_B.output_path]),
        TableEntry(
            id=DERIVATION_B.output_path,
            neighbours=[DERIVATION_AA.output_path, DERIVATION_C.output_path],
        ),
        TableEntry(id=DERIVATION_AA.output_path, neighbours=[]),
        TableEntry(id=DERIVATION_C.output_path, neighbours=[]),
    ]
    expected_edges = [
        EdgeDefinition(
            data=EdgeData(
                source=DERIVATION_A.output_path, target=DERIVATION_B.output_path
            )
        ),
        EdgeDefinition(
            data=EdgeData(
                source=DERIVATION_B.output_path, target=DERIVATION_AA.output_path
            )
        ),
        EdgeDefinition(
            data=EdgeData(
                source=DERIVATION_B.output_path, target=DERIVATION_C.output_path
            )
        ),
    ]
    expected_nodes = [
        NodeDefinition(data=NodeData(id=DERIVATION_A.output_path)),
        NodeDefinition(data=NodeData(id=DERIVATION_B.output_path)),
        NodeDefinition(data=NodeData(id=DERIVATION_AA.output_path)),
        NodeDefinition(data=NodeData(id=DERIVATION_C.output_path)),
    ]
    expected_graph_data = CytoscapeJs(
        elements=ElementsDefinition(nodes=expected_nodes, edges=expected_edges)
    )

    assert result.cyto is not None
    assert result.cyto.table_data == expected_table_data
    assert result.cyto.graph_data == expected_graph_data
    # Note: Not including an assertion for `raw` property since that value is hard
    # to construct manually for a more complex query such as the one in this test.

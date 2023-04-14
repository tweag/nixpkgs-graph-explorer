from typing import Any, Mapping, cast

import pytest
from gremlin_python.driver import serializer
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.anonymous_traversal import traversal

from explorer.api.graph import ElementId, GraphElement, UniqueGraphElement


class DummyGraphElement(GraphElement):
    """
    Dummy graph element for testing purposes
    """

    @classmethod
    def label(cls) -> str:
        return "dummy-element"


class DummyUniqueGraphElement(UniqueGraphElement):
    """
    Dummy unique graph element for testing purposes
    """

    unique_property: str

    @classmethod
    def label(cls) -> str:
        return "unique-dummy-element"

    @classmethod
    def id_property_name(cls) -> str:
        return "unique_property"

    def get_id(self) -> str:
        return ElementId(self.unique_property)


class DummyEdge(GraphElement):
    """
    Dummy edge element for testing purposes
    """

    prop1: str
    prop2: int

    @classmethod
    def label(cls) -> str:
        return "dummy-edge"


class DummyEdgeNoProps(GraphElement):
    """
    Dummy edge element for testing purposes which has no properties
    """

    @classmethod
    def label(cls) -> str:
        return "dummy-edge-no-props"


@pytest.fixture
def graph_connection():
    conn = DriverRemoteConnection(
        "ws://localhost:8182/gremlin",
        "g",
        message_serializer=serializer.GraphSONMessageSerializer(),
    )
    g = traversal().with_remote(conn)
    yield conn
    # Dropping all data from graph on teardown
    g.V().drop().iterate()
    g.E().drop().iterate()
    conn.close()


def test_unit_insert_vertex(graph_connection: DriverRemoteConnection):
    from explorer.api.graph import insert_vertex

    g = traversal().withRemote(graph_connection)
    element = DummyGraphElement()
    insert_vertex(element, g)
    assert g.V().count().to_list() == [1]


def test_unit_insert_vertex_allows_duplicates(graph_connection: DriverRemoteConnection):
    from explorer.api.graph import insert_vertex

    g = traversal().withRemote(graph_connection)
    element = DummyGraphElement()
    insert_vertex(element, g)
    insert_vertex(element, g)
    assert g.V().count().to_list() == [2]


def test_unit_insert_unique_vertex_creates_when_one_does_not_exist(
    graph_connection: DriverRemoteConnection,
):
    from explorer.api.graph import insert_unique_vertex

    g = traversal().withRemote(graph_connection)
    element = DummyUniqueGraphElement(unique_property="some-unique-value-1")
    insert_unique_vertex(element, g)
    assert g.V().count().to_list() == [1]


def test_unit_insert_unique_vertex_does_not_duplicate_vertex(
    graph_connection: DriverRemoteConnection,
):
    from explorer.api.graph import insert_unique_vertex

    g = traversal().withRemote(graph_connection)
    element = DummyUniqueGraphElement(unique_property="some-unique-value-1")
    insert_unique_vertex(element, g)
    insert_unique_vertex(element, g)
    assert g.V().count().to_list() == [1]


def test_unit_insert_unique_directed_edge_both_vertices_exist(
    graph_connection: DriverRemoteConnection,
):
    from explorer.api.graph import insert_unique_directed_edge, insert_unique_vertex

    # Pre-populate the graph with vertices
    g = traversal().withRemote(graph_connection)
    vertex1 = DummyUniqueGraphElement(unique_property="some-unique-value-1")
    vertex2 = DummyUniqueGraphElement(unique_property="some-unique-value-2")
    insert_unique_vertex(vertex1, g)
    insert_unique_vertex(vertex2, g)

    # Create the edge
    insert_unique_directed_edge(
        DummyEdgeNoProps(), from_vertex=vertex1, to_vertex=vertex2, g=g
    )
    assert g.E().count().to_list() == [1]


def test_unit_insert_unique_directed_edge_does_nothing_if_from_vertex_not_found(
    graph_connection: DriverRemoteConnection,
):
    from explorer.api.graph import insert_unique_directed_edge, insert_unique_vertex

    # Pre-populate the graph with a single vertex. The other one is "missing".
    g = traversal().withRemote(graph_connection)
    vertex = DummyUniqueGraphElement(unique_property="some-unique-value-1")
    missing_vertex = DummyUniqueGraphElement(unique_property="some-unique-value-2")
    insert_unique_vertex(vertex, g)

    # Create the edge
    insert_unique_directed_edge(
        DummyEdgeNoProps(), from_vertex=missing_vertex, to_vertex=vertex, g=g
    )
    assert g.E().count().to_list() == [0]


def test_unit_insert_unique_directed_edge_does_nothing_if_to_vertex_not_found(
    graph_connection: DriverRemoteConnection,
):
    from explorer.api.graph import insert_unique_directed_edge, insert_unique_vertex

    # Pre-populate the graph with a single vertex. The other one is "missing".
    g = traversal().withRemote(graph_connection)
    vertex = DummyUniqueGraphElement(unique_property="some-unique-value-1")
    missing_vertex = DummyUniqueGraphElement(unique_property="some-unique-value-2")
    insert_unique_vertex(vertex, g)

    # Create the edge
    insert_unique_directed_edge(
        DummyEdgeNoProps(), from_vertex=vertex, to_vertex=missing_vertex, g=g
    )
    assert g.E().count().to_list() == [0]


def test_unit_insert_unique_directed_edge_does_not_create_duplicates(
    graph_connection: DriverRemoteConnection,
):
    from explorer.api.graph import insert_unique_directed_edge, insert_unique_vertex

    # Pre-populate the graph with vertices
    g = traversal().withRemote(graph_connection)
    vertex1 = DummyUniqueGraphElement(unique_property="some-unique-value-1")
    vertex2 = DummyUniqueGraphElement(unique_property="some-unique-value-2")
    insert_unique_vertex(vertex1, g)
    insert_unique_vertex(vertex2, g)

    # Create the edge
    insert_unique_directed_edge(
        DummyEdgeNoProps(), from_vertex=vertex1, to_vertex=vertex2, g=g
    )
    # Attempt to insert it again
    insert_unique_directed_edge(
        DummyEdgeNoProps(), from_vertex=vertex1, to_vertex=vertex2, g=g
    )

    assert g.E().count().to_list() == [1]


def test_unit_insert_unique_directed_edge_created_edge_properties(
    graph_connection: DriverRemoteConnection,
):
    from explorer.api.graph import insert_unique_directed_edge, insert_unique_vertex

    # Pre-populate the graph with vertices
    g = traversal().withRemote(graph_connection)
    vertex1 = DummyUniqueGraphElement(unique_property="some-unique-value-1")
    vertex2 = DummyUniqueGraphElement(unique_property="some-unique-value-2")
    insert_unique_vertex(vertex1, g)
    insert_unique_vertex(vertex2, g)

    # Create the edge
    edge = DummyEdge(prop1="foo", prop2=1)
    insert_unique_directed_edge(edge, from_vertex=vertex1, to_vertex=vertex2, g=g)
    # Attempt to read the edge's properties
    element_maps = cast(
        Mapping[Any, Any], g.E().hasLabel(edge.label()).element_map().to_list()
    )
    assert [DummyEdge.from_element_map(em) for em in element_maps] == [edge]

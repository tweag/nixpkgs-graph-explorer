import pytest
from gremlin_python.driver import serializer
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.anonymous_traversal import traversal

from explorer.graph import ElementId, GraphElement, UniqueGraphElement


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
    from explorer.graph import insert_vertex

    g = traversal().withRemote(graph_connection)
    element = DummyGraphElement()
    insert_vertex(element, g)
    assert g.V().count().to_list() == [1]


def test_unit_insert_vertex_allows_duplicates(graph_connection: DriverRemoteConnection):
    from explorer.graph import insert_vertex

    g = traversal().withRemote(graph_connection)
    element = DummyGraphElement()
    insert_vertex(element, g)
    insert_vertex(element, g)
    assert g.V().count().to_list() == [2]


def test_unit_insert_unique_vertex_creates_when_one_does_not_exist(
    graph_connection: DriverRemoteConnection,
):
    from explorer.graph import insert_unique_vertex

    g = traversal().withRemote(graph_connection)
    element = DummyUniqueGraphElement(unique_property="some-unique-value-1")
    insert_unique_vertex(element, g)
    assert g.V().count().to_list() == [1]


def test_unit_insert_unique_vertex_does_not_duplicate_vertex(
    graph_connection: DriverRemoteConnection,
):
    from explorer.graph import insert_unique_vertex

    g = traversal().withRemote(graph_connection)
    element = DummyUniqueGraphElement(unique_property="some-unique-value-1")
    insert_unique_vertex(element, g)
    insert_unique_vertex(element, g)
    assert g.V().count().to_list() == [1]

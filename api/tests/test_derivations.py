import pytest
from gremlin_python.process.anonymous_traversal import traversal

import explorer.api.queries.derivations as derivations
from explorer.api.graph import Derivation, insert_unique_vertex
from explorer.api.gremlin import default_remote_connection
from explorer.api.queries.pagination import Cursor, CursorDirection

DERIVATION_A = Derivation(output_path="/nix/store/a", attribute_path="a")
DERIVATION_B = Derivation(output_path="/nix/store/b", attribute_path="b")
DERIVATION_C = Derivation(output_path="/nix/store/c", attribute_path="c")
DERIVATION_AA = Derivation(output_path="/nix/store/aa", attribute_path="aa")

DUMMY_DERIVATIONS = [DERIVATION_A, DERIVATION_B, DERIVATION_C, DERIVATION_AA]


@pytest.fixture
def graph_connection():
    # FIXME: use a traversal_source purely dedicated to tests
    conn = default_remote_connection(
        "ws://localhost:8182/gremlin", traversal_source="g"
    )
    g = traversal().with_remote(conn)
    # Dropping all data from graph on start
    g.V().drop().iterate()
    g.E().drop().iterate()
    # Insert test dataset
    for drv in DUMMY_DERIVATIONS:
        insert_unique_vertex(g, drv)
    yield conn
    # Dropping all data from graph on teardown
    g.V().drop().iterate()
    g.E().drop().iterate()
    conn.close()


def test_unit_name_filter_multi_matches(graph_connection):
    cursor = None
    search_predicate = "a"
    request = derivations.ListDerivationsRequest(
        cursor=cursor, search_predicate=search_predicate
    )
    response = derivations.list_derivations(request, graph_connection)
    expected_response = derivations.ListDerivationsResponse(
        new_cursor=None,
        derivations=[DERIVATION_A, DERIVATION_AA],
    )
    assert response == expected_response


def test_unit_name_filter_single_match(graph_connection):
    cursor = None
    search_predicate = "b"
    request = derivations.ListDerivationsRequest(
        cursor=cursor, search_predicate=search_predicate
    )
    response = derivations.list_derivations(request, graph_connection)
    expected_response = derivations.ListDerivationsResponse(
        new_cursor=None,
        derivations=[DERIVATION_B],
    )
    assert response == expected_response


def test_unit_name_filter_no_match(graph_connection):
    cursor = None
    search_predicate = "does-not-exist"
    request = derivations.ListDerivationsRequest(
        cursor=cursor, search_predicate=search_predicate
    )
    response = derivations.list_derivations(request, graph_connection)
    expected_response = derivations.ListDerivationsResponse(
        new_cursor=None,
        derivations=[],
    )
    assert response == expected_response


def test_unit_cursor_direction_previous(graph_connection):
    cursor = Cursor.from_unique_element(
        DERIVATION_B, direction=CursorDirection.PREVIOUS
    )
    search_predicate = None
    request = derivations.ListDerivationsRequest(
        cursor=cursor, search_predicate=search_predicate, limit=2
    )
    response = derivations.list_derivations(request, graph_connection)
    expected_response = derivations.ListDerivationsResponse(
        new_cursor=Cursor.from_unique_element(
            DERIVATION_A, direction=CursorDirection.PREVIOUS
        ),
        derivations=[DERIVATION_B, DERIVATION_AA],
    )
    assert response == expected_response


def test_unit_cursor_direction_next(graph_connection):
    cursor = Cursor.from_unique_element(DERIVATION_B, direction=CursorDirection.NEXT)
    search_predicate = None
    request = derivations.ListDerivationsRequest(
        cursor=cursor, search_predicate=search_predicate, limit=2
    )
    response = derivations.list_derivations(request, graph_connection)
    expected_response = derivations.ListDerivationsResponse(
        new_cursor=None,
        derivations=[DERIVATION_B, DERIVATION_C],
    )
    assert response == expected_response


def test_unit_page_size_lt_total(graph_connection):
    cursor = None
    search_predicate = None
    request = derivations.ListDerivationsRequest(
        cursor=cursor, search_predicate=search_predicate, limit=2
    )
    response = derivations.list_derivations(request, graph_connection)
    expected_response = derivations.ListDerivationsResponse(
        new_cursor=Cursor.from_unique_element(DERIVATION_B),
        derivations=[DERIVATION_A, DERIVATION_AA],
    )
    assert response == expected_response


def test_unit_page_size_eq_total(graph_connection):
    cursor = None
    search_predicate = None
    request = derivations.ListDerivationsRequest(
        cursor=cursor, search_predicate=search_predicate, limit=4
    )
    response = derivations.list_derivations(request, graph_connection)
    expected_response = derivations.ListDerivationsResponse(
        new_cursor=None,
        derivations=[
            DERIVATION_A,
            DERIVATION_AA,
            DERIVATION_B,
            DERIVATION_C,
        ],
    )
    assert response == expected_response


def test_unit_page_size_gt_total(graph_connection):
    cursor = None
    search_predicate = None
    request = derivations.ListDerivationsRequest(
        cursor=cursor, search_predicate=search_predicate, limit=5
    )
    response = derivations.list_derivations(request, graph_connection)
    expected_response = derivations.ListDerivationsResponse(
        new_cursor=None,
        derivations=[
            DERIVATION_A,
            DERIVATION_AA,
            DERIVATION_B,
            DERIVATION_C,
        ],
    )
    assert response == expected_response

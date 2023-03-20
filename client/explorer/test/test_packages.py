from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
import pytest
from gremlin_python.driver import serializer
import explorer.queries.packages as packages

from explorer.queries.pagination import Cursor, CursorDirection
from gremlin_python.process.anonymous_traversal import traversal
from explorer.graph import insert_unique_vertex, Package

PACKAGE_A = Package("package-a", "a/a/a")
PACKAGE_B = Package("package-b", "b/b/b")
PACKAGE_C = Package("package-c", "c/c/c")
PACKAGE_AA = Package("package-aa", "aa/aa/aa")

DUMMY_PACKAGES = [PACKAGE_A, PACKAGE_B, PACKAGE_C, PACKAGE_AA]


@pytest.fixture
def graph_connection():
    conn = DriverRemoteConnection(
        "ws://localhost:8182/gremlin",
        "g",
        message_serializer=serializer.GraphSONMessageSerializer(),
    )
    g = traversal().with_remote(conn)
    # Insert test dataset
    for p in DUMMY_PACKAGES:
        insert_unique_vertex(p, g)
    yield conn
    # Dropping all data from graph on teardown
    g.V().drop().iterate()
    g.E().drop().iterate()
    conn.close()


def test_unit_name_filter_multi_matches(graph_connection):
    cursor = None
    search_predicate = "package-a"
    request = packages.ListPackagesRequest(cursor, search_predicate)
    response = packages.list_packages(request, graph_connection)
    expected_response = packages.ListPackagesResponse(
        None,
        [PACKAGE_A, PACKAGE_AA],
    )
    assert response == expected_response


def test_unit_name_filter_single_match(graph_connection):
    cursor = None
    search_predicate = "package-b"
    request = packages.ListPackagesRequest(cursor, search_predicate)
    response = packages.list_packages(request, graph_connection)
    expected_response = packages.ListPackagesResponse(
        None,
        [PACKAGE_B],
    )
    assert response == expected_response


def test_unit_name_filter_no_match(graph_connection):
    cursor = None
    search_predicate = "does-not-exist"
    request = packages.ListPackagesRequest(cursor, search_predicate)
    response = packages.list_packages(request, graph_connection)
    expected_response = packages.ListPackagesResponse(
        None,
        [],
    )
    assert response == expected_response


def test_unit_cursor_direction_previous(graph_connection):
    cursor = Cursor.from_unique_element(PACKAGE_B, direction=CursorDirection.PREVIOUS)
    search_predicate = None
    request = packages.ListPackagesRequest(cursor, search_predicate, limit=2)
    response = packages.list_packages(request, graph_connection)
    expected_response = packages.ListPackagesResponse(
        Cursor.from_unique_element(PACKAGE_A, direction=CursorDirection.PREVIOUS),
        [PACKAGE_B, PACKAGE_AA],
    )
    assert response == expected_response


def test_unit_cursor_direction_next(graph_connection):
    cursor = Cursor.from_unique_element(PACKAGE_B, direction=CursorDirection.NEXT)
    search_predicate = None
    request = packages.ListPackagesRequest(cursor, search_predicate, limit=2)
    response = packages.list_packages(request, graph_connection)
    expected_response = packages.ListPackagesResponse(
        None,
        [PACKAGE_B, PACKAGE_C],
    )
    assert response == expected_response


def test_unit_page_size_lt_total(graph_connection):
    cursor = None
    search_predicate = None
    request = packages.ListPackagesRequest(cursor, search_predicate, limit=2)
    response = packages.list_packages(request, graph_connection)
    expected_response = packages.ListPackagesResponse(
        Cursor.from_unique_element(PACKAGE_B),
        [PACKAGE_A, PACKAGE_AA],
    )
    assert response == expected_response


def test_unit_page_size_eq_total(graph_connection):
    cursor = None
    search_predicate = None
    request = packages.ListPackagesRequest(cursor, search_predicate, limit=4)
    response = packages.list_packages(request, graph_connection)
    expected_response = packages.ListPackagesResponse(
        None,
        [
            PACKAGE_A,
            PACKAGE_AA,
            PACKAGE_B,
            PACKAGE_C,
        ],
    )
    assert response == expected_response


def test_unit_page_size_gt_total(graph_connection):
    cursor = None
    search_predicate = None
    request = packages.ListPackagesRequest(cursor, search_predicate, limit=5)
    response = packages.list_packages(request, graph_connection)
    expected_response = packages.ListPackagesResponse(
        None,
        [
            PACKAGE_A,
            PACKAGE_AA,
            PACKAGE_B,
            PACKAGE_C,
        ],
    )
    assert response == expected_response

import pytest
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection

import explorer.api.queries.derivations as derivations
from explorer.api.graph import Derivation
from explorer.api.queries.pagination import Cursor, CursorDirection
from tests.constants import DERIVATION_A, DERIVATION_AA, DERIVATION_B, DERIVATION_C


@pytest.mark.parametrize(
    "search_predicate,expected_derivations",
    [
        ("a", [DERIVATION_A, DERIVATION_AA]),
        ("b", [DERIVATION_B]),
        ("does-not-exist", []),
    ],
)
def test_unit_name_filter_matches(
    populated_graph_connection: DriverRemoteConnection,
    search_predicate: str,
    expected_derivations: list[Derivation],
):
    cursor = None
    request = derivations.ListDerivationsRequest(
        cursor=cursor, search_predicate=search_predicate
    )
    response = derivations.list_derivations(request, populated_graph_connection)
    expected_response = derivations.ListDerivationsResponse(
        new_cursor=None,
        derivations=expected_derivations,
    )
    assert response == expected_response


@pytest.mark.parametrize(
    "cursor,expected_cursor,expected_derivations",
    [
        (
            Cursor.from_unique_element(
                DERIVATION_B, direction=CursorDirection.PREVIOUS
            ),
            Cursor.from_unique_element(
                DERIVATION_A, direction=CursorDirection.PREVIOUS
            ),
            [DERIVATION_B, DERIVATION_AA],
        ),
        (
            Cursor.from_unique_element(DERIVATION_B, direction=CursorDirection.NEXT),
            None,
            [DERIVATION_B, DERIVATION_C],
        ),
    ],
)
def test_unit_cursor(
    populated_graph_connection: DriverRemoteConnection,
    cursor: Cursor,
    expected_cursor: Cursor,
    expected_derivations: list[Derivation],
):
    search_predicate = None
    request = derivations.ListDerivationsRequest(
        cursor=cursor,
        search_predicate=search_predicate,
        limit=2,
    )
    response = derivations.list_derivations(request, populated_graph_connection)
    expected_response = derivations.ListDerivationsResponse(
        new_cursor=expected_cursor,
        derivations=expected_derivations,
    )
    assert response == expected_response


def test_unit_page_size_lt_total(populated_graph_connection):
    cursor = None
    search_predicate = None
    request = derivations.ListDerivationsRequest(
        cursor=cursor, search_predicate=search_predicate, limit=2
    )
    response = derivations.list_derivations(request, populated_graph_connection)
    expected_response = derivations.ListDerivationsResponse(
        new_cursor=Cursor.from_unique_element(DERIVATION_B),
        derivations=[DERIVATION_A, DERIVATION_AA],
    )
    assert response == expected_response


def test_unit_page_size_eq_total(populated_graph_connection):
    cursor = None
    search_predicate = None
    request = derivations.ListDerivationsRequest(
        cursor=cursor, search_predicate=search_predicate, limit=4
    )
    response = derivations.list_derivations(request, populated_graph_connection)
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


def test_unit_page_size_gt_total(populated_graph_connection):
    cursor = None
    search_predicate = None
    request = derivations.ListDerivationsRequest(
        cursor=cursor, search_predicate=search_predicate, limit=5
    )
    response = derivations.list_derivations(request, populated_graph_connection)
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

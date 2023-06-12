import logging
from typing import Optional

from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.process.graph_traversal import GraphTraversal
from gremlin_python.process.traversal import Order, TextP
from pydantic import BaseModel, Field

from explorer.api.graph import Derivation
from explorer.api.queries.pagination import Cursor, CursorDirection

logger = logging.getLogger(__name__)


# @dataclass
class ListDerivationsRequest(BaseModel):
    cursor: Optional[Cursor] = None
    search_predicate: Optional[str] = Field(
        description="Regex to use to search derivation",
    )
    limit: int = 10


# @dataclass
class ListDerivationsResponse(BaseModel):
    new_cursor: Optional[Cursor]
    derivations: list[Derivation]


def list_derivations(
    request: ListDerivationsRequest, conn: DriverRemoteConnection
) -> ListDerivationsResponse:
    """Lists derivations available in the specified Gremlin Server

    Args:
        request (ListDerivationsRequest): The request parameters
        conn (DriverRemoteConnection): A Gremlin Driver remote connection

    Returns:
        ListDerivationsResponse: A new cursor (if available) and the
        list of derivations matching the provided request.
    """
    # Since the provided search predicate filters by name, we need to know the name
    # of the relevant vertex property. There might be smarter ways to extract this
    # rather than hard-coding it here.
    search_property = Derivation.id_property_name()

    # Connect to Gremlin Server and start a new traversal
    g = traversal().withRemote(conn)

    # First filter traversal based on predicate if one exists
    filtered_traversal: GraphTraversal
    if request.search_predicate:
        logger.debug(
            "Filtering traversal using predicate: %s",
            request.search_predicate,
        )
        filtered_traversal = g.V().has(
            search_property, TextP.containing(request.search_predicate)
        )
    else:
        filtered_traversal = g.V().order().by(search_property)

    # Apply ordering to the traversal
    if not request.cursor or request.cursor.direction is CursorDirection.NEXT:
        ordering = Order.asc  # type: ignore
    else:
        ordering = Order.desc  # type: ignore
    ordered_filtered_traversal = filtered_traversal.order().by(
        search_property, ordering
    )

    # Then we can filter based on the cursor, if one exists.
    if request.cursor:
        if request.cursor.direction == CursorDirection.NEXT:
            logger.debug("cursor direction=next")
            cursor_predicate = TextP.gte(request.cursor.row_id)
        else:
            logger.debug("cursor direction=previous")
            cursor_predicate = TextP.lte(request.cursor.row_id)
        page_traversal = (
            ordered_filtered_traversal.has(
                Derivation.id_property_name(), cursor_predicate
            )
            .limit(request.limit + 1)
            .element_map()
        )
    else:
        logger.debug("Querying without cursor.")
        # Note: Explicitly not sorting the results here since ordering steps in Gremlin
        # can sometimes require that the server reads the entire traversal which can be
        # inefficient.
        page_traversal = ordered_filtered_traversal.limit(
            request.limit + 1
        ).element_map()

    # Execute traversal and parse results
    derivations = [Derivation.from_element_map(em) for em in page_traversal.to_list()]

    if not derivations:
        return ListDerivationsResponse(new_cursor=None, derivations=[])

    # If result set has same length as limit, we have reached the end of the result set
    # and do not need to return a new cursor.
    if len(derivations) <= request.limit:
        return ListDerivationsResponse(new_cursor=None, derivations=derivations)

    # Otherwise, construct the new cursor from the last item in the query result and
    # return all other items.
    new_cursor_direction = (
        CursorDirection.NEXT if not request.cursor else request.cursor.direction
    )
    new_cursor = Cursor.from_unique_element(
        derivations[-1], direction=new_cursor_direction
    )
    # Note: Ignoring last item in result set since it is only used for constructing
    # our new cursor
    return ListDerivationsResponse(new_cursor=new_cursor, derivations=derivations[:-1])

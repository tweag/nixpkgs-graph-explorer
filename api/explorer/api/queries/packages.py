import logging
from typing import Optional

from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.process.traversal import Order, TextP
from pydantic import BaseModel

from explorer.api.graph import Package
from explorer.api.queries.pagination import Cursor, CursorDirection

logger = logging.getLogger(__name__)

# Since the provided search predicate filters by name, we need to know the name
# of the relevant vertex property. There might be smarter ways to extract this
# rather than hard-coding it here.
NAME_PROPERTY = "pname"


class ListPackagesRequest(BaseModel):
    cursor: Optional[Cursor] = None
    search_predicate: Optional[str] = None
    limit: int = 10


class ListPackagesResponse(BaseModel):
    new_cursor: Optional[Cursor]
    packages: list[Package]


class ListUniquePackageNamesRequest(BaseModel):
    cursor: Optional[Cursor] = None
    search_predicate: Optional[str] = None
    limit: int = 10


class ListUniquePackageNamesResponse(BaseModel):
    new_cursor: Optional[Cursor]
    package_names: list[str]


def list_unique_package_names(
    request: ListUniquePackageNamesRequest, conn: DriverRemoteConnection
) -> ListUniquePackageNamesResponse:
    """Lists the unique names of packages available in the specified Gremlin Server

    Args:
        request (ListUniquePackageNamesRequest): The request parameters
        conn (DriverRemoteConnection): A Gremlin Driver remote connection

    Returns:
        ListUniquePackageNamesResponse: A new cursor (if available) and the
        list of package names matching the provided request.
    """

    # Connect to Gremlin Server and start a new traversal
    g = traversal().withRemote(conn)

    # First filter traversal based on predicate if one exists
    filtered_traversal = g.V().values(NAME_PROPERTY)
    if request.search_predicate:
        logger.debug("Filtering traversal using provided predicate.")
        filtered_traversal = filtered_traversal.is_(
            TextP.startingWith(request.search_predicate)
        )

    # Apply ordering to the traversal
    if not request.cursor or request.cursor.direction is CursorDirection.NEXT:
        ordering = Order.asc  # type: ignore
    else:
        ordering = Order.desc  # type: ignore
    ordered_filtered_traversal = filtered_traversal.order().by(ordering)

    # Then we can filter based on the cursor, if one exists.
    if request.cursor:
        if request.cursor.direction == CursorDirection.NEXT:
            logger.debug("cursor direction=next")
            cursor_predicate = TextP.gte(request.cursor.row_id)
        else:
            logger.debug("cursor direction=previous")
            cursor_predicate = TextP.lte(request.cursor.row_id)
        page_traversal = (
            ordered_filtered_traversal.is_(cursor_predicate)
            .dedup()
            .limit(request.limit + 1)
        )
    else:
        logger.debug("Querying without cursor.")
        # Note: Explicitly not sorting the results here since ordering steps in Gremlin
        # can sometimes require that the server reads the entire traversal which can be
        # inefficient.
        page_traversal = ordered_filtered_traversal.dedup().limit(request.limit + 1)

    # Execute traversal and parse results
    package_names = page_traversal.to_list()

    if not package_names:
        return ListUniquePackageNamesResponse(new_cursor=None, package_names=[])

    # If result set has same length as limit, we have reached the end of the result set
    # and do not need to return a new cursor.
    if len(package_names) <= request.limit:
        return ListUniquePackageNamesResponse(
            new_cursor=None, package_names=package_names
        )

    # Otherwise, construct the new cursor from the last item in the query result and
    # return all other items.
    new_cursor_direction = (
        CursorDirection.NEXT if not request.cursor else request.cursor.direction
    )
    new_cursor = Cursor(row_id=package_names[-1], direction=new_cursor_direction)
    # Note: Ignoring last item in result set since it is only used for constructing
    # our new cursor
    return ListUniquePackageNamesResponse(
        new_cursor=new_cursor, package_names=package_names[:-1]
    )


def list_packages(
    request: ListPackagesRequest, conn: DriverRemoteConnection
) -> ListPackagesResponse:
    """Lists packages available in the specified Gremlin Server

    Args:
        request (ListPackagesRequest): The request parameters
        conn (DriverRemoteConnection): A Gremlin Driver remote connection

    Returns:
        ListPackagesResponse: A new cursor (if available) and the
        list of packages matching the provided request.
    """
    # Since the provided search predicate filters by name, we need to know the name
    # of the relevant vertex property. There might be smarter ways to extract this
    # rather than hard-coding it here.
    NAME_PROPERTY = "pname"

    # Connect to Gremlin Server and start a new traversal
    g = traversal().withRemote(conn)

    # First filter traversal based on predicate if one exists
    if request.search_predicate:
        logger.debug("Filtering traversal using provided predicate.")
        filtered_traversal = g.V().has(
            NAME_PROPERTY, TextP.startingWith(request.search_predicate)
        )
    else:
        filtered_traversal = g.V().order().by(NAME_PROPERTY)

    # Apply ordering to the traversal
    if not request.cursor or request.cursor.direction is CursorDirection.NEXT:
        ordering = Order.asc  # type: ignore
    else:
        ordering = Order.desc  # type: ignore
    ordered_filtered_traversal = filtered_traversal.order().by(NAME_PROPERTY, ordering)

    # Then we can filter based on the cursor, if one exists.
    if request.cursor:
        if request.cursor.direction == CursorDirection.NEXT:
            logger.debug("cursor direction=next")
            cursor_predicate = TextP.gte(request.cursor.row_id)
        else:
            logger.debug("cursor direction=previous")
            cursor_predicate = TextP.lte(request.cursor.row_id)
        page_traversal = (
            ordered_filtered_traversal.has(Package.id_property_name(), cursor_predicate)
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
    packages = [Package.from_element_map(em) for em in page_traversal.to_list()]

    if not packages:
        return ListPackagesResponse(new_cursor=None, packages=[])

    # If result set has same length as limit, we have reached the end of the result set
    # and do not need to return a new cursor.
    if len(packages) <= request.limit:
        return ListPackagesResponse(new_cursor=None, packages=packages)

    # Otherwise, construct the new cursor from the last item in the query result and
    # return all other items.
    new_cursor_direction = (
        CursorDirection.NEXT if not request.cursor else request.cursor.direction
    )
    new_cursor = Cursor.from_unique_element(
        packages[-1], direction=new_cursor_direction
    )
    # Note: Ignoring last item in result set since it is only used for constructing
    # our new cursor
    return ListPackagesResponse(new_cursor=new_cursor, packages=packages[:-1])

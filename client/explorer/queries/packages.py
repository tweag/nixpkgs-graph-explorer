from dataclasses import dataclass
from typing import Any, Dict, List, Optional
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.process.traversal import TextP, T

import marshmallow_dataclass

from explorer.queries.pagination import Cursor, CursorDirection
from explorer.graph import Package


@dataclass
class PackageInfo:
    name: str


@dataclass
class ListPackagesRequest:
    cursor: Optional[Cursor] = None
    search_predicate: Optional[str] = None
    limit: int = 10


@dataclass
class ListPackagesResponse:
    new_cursor: Optional[Cursor]
    packages: List[PackageInfo]


# marshmallow schemas
ListPackagesRequestSchema = marshmallow_dataclass.class_schema(ListPackagesRequest)
ListPackagesResponseSchema = marshmallow_dataclass.class_schema(ListPackagesResponse)


# Helper for filtering Gremlin's id and label fields from a Gremlin element map
def _element_map_to_properties(element_map: dict[Any, Any]) -> Dict[str, Any]:
    return {k: v for k, v in element_map.items() if k != T.id and k != T.label}


def list_packages(
    request: ListPackagesRequest, conn: DriverRemoteConnection
) -> ListPackagesResponse:
    # Name of the field containing package names. This is treated as a unique identifier.
    NAME_FIELD = "pname"

    # Connect to Gremlin Server and start a new traversal
    g = traversal().withRemote(conn)

    # First traverse based on predicate if one exists
    if request.search_predicate:
        print("Filtering traversal using provided predicate.")
        filtered_traversal = g.V().has(
            NAME_FIELD, TextP.startingWith(request.search_predicate)
        )
    else:
        filtered_traversal = g.V()
    # Then we can filter based on the cursor, if one exists.
    if request.cursor:
        print("Using cursor")
        if request.cursor.direction == CursorDirection.NEXT:
            print("cursor direction=next")
            cursor_predicate = TextP.gte(request.cursor.row_id)
        else:
            print("cursor direction=previous")
            cursor_predicate = TextP.lte(request.cursor.row_id)
        page_traversal = (
            filtered_traversal.has(NAME_FIELD, cursor_predicate)
            .limit(request.limit + 1)
            .element_map()
        )
    else:
        print("Querying without cursor.")
        # Note: Explicitly not sorting the results here since ordering steps in Gremlin can sometimes
        # require that the server reads the entire traversal which can be inefficient.
        page_traversal = filtered_traversal.limit(request.limit + 1).element_map()

    packages = [Package.from_element_map(em) for em in page_traversal.to_list()]
    # Only return relevant package data
    package_info = [PackageInfo(p.pname) for p in packages]
    if not packages:
        return ListPackagesResponse(None, [])

    # Defaulting to NEXT direction if no cursor was provided
    new_cursor_direction = (
        CursorDirection.NEXT if not request.cursor else request.cursor.direction
    )
    new_cursor = Cursor(package_info[-1].name, direction=new_cursor_direction)
    # Note: Ignoring last item in result set since it is only used for constructing our new cursor
    return ListPackagesResponse(new_cursor, package_info[:-1])

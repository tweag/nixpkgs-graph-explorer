"""nixpkgs-graph-explorer FastAPI app."""
import os
from contextlib import asynccontextmanager
from typing import AsyncGenerator, TypedDict, cast

import dotenv
from fastapi import FastAPI, Request
from gremlin_python.driver import serializer
from gremlin_python.driver.client import Client
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from pydantic import BaseModel
from starlette.applications import Starlette

from explorer.queries.packages import (
    ListPackagesRequest,
    ListPackagesResponse,
    list_packages,
)
from explorer.queries.query import GremlinResult, QueryResult

##############################################################################
# Configuration
##############################################################################

# TODO: Make this configurable
READ_ONLY_TRAVERSAL_SOURCE = "gReadOnly"

# First, load variables from .env file if one exists
dotenv.load_dotenv(override=True)
# Then we can read configuration from environment variables
gremlin_host = os.getenv(key="GREMLIN_HOST", default="localhost")
gremlin_port = os.getenv(key="GREMLIN_PORT", default="8182")


##############################################################################
# Application state. Used for stuff like database clients which need to be
# initialized at load time.
##############################################################################


class State(TypedDict):
    gremlin_client: Client
    gremlin_remote_connection: DriverRemoteConnection


@asynccontextmanager
async def lifespan(_app: Starlette) -> AsyncGenerator[State, None]:
    """Context manager for managing Starlett (FastAPI) application state.

    This context manager will ensure all clients are properly closed on
    application shutdown.

    Yields:
        Iterator[AsyncGenerator[State, None]]: The initialized application state
    """
    # Set-up resources for application
    # FIXME: The below two Gremlin objects have blocking constructors
    gremlin_url = f"ws://{gremlin_host}:{gremlin_port}/gremlin"
    gremlin_client = Client(
        gremlin_url,
        READ_ONLY_TRAVERSAL_SOURCE,
        pool_size=4,
        message_serializer=serializer.GraphSONMessageSerializer(),
    )
    gremlin_remote_connection = DriverRemoteConnection(
        gremlin_url,
        READ_ONLY_TRAVERSAL_SOURCE,
        message_serializer=serializer.GraphSONMessageSerializer(),
    )
    yield {
        "gremlin_client": gremlin_client,
        "gremlin_remote_connection": gremlin_remote_connection,
    }
    # Tear down resources on application shut-down
    gremlin_client.close()
    gremlin_remote_connection.close()


def get_state(request: Request) -> State:
    """Helper for getting the FastAPI application state from a Request.

    Assumes that the lifespan has been configured with the lifespan
    context manager set in this module.

    Args:
        request (Request): A FastAPI Request

    Returns:
        State: The application state
    """
    return cast(State, request.state._state)


##############################################################################
# Application Definition
##############################################################################


# TODO: Move this into the queries.query module
class GremlinRequest(BaseModel):
    query: str


app = FastAPI(lifespan=lifespan)


@app.post("/packages")
def packages(
    list_packages_request: ListPackagesRequest, raw_request: Request
) -> ListPackagesResponse:
    remote_connection = get_state(raw_request)["gremlin_remote_connection"]
    return list_packages(list_packages_request, remote_connection)


@app.post("/gremlin")
def gremlin(gremlin_request: GremlinRequest, raw_request: Request) -> QueryResult:
    client = get_state(raw_request)["gremlin_client"]
    query_result = GremlinResult(client, gremlin_request.query, clean_gremlin=True)
    return query_result.to_query_result()

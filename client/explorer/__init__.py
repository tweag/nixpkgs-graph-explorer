"""Initialize Flask app."""
from dataclasses import dataclass

from flask import Flask, jsonify, request
from gremlin_python.driver import serializer
from gremlin_python.driver.client import Client
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from marshmallow import ValidationError

from explorer.queries import query
from explorer.queries.packages import (
    ListPackagesRequest,
    ListPackagesRequestSchema,
    ListPackagesResponseSchema,
    list_packages,
)

READ_ONLY_TRAVERSAL_SOURCE = "gReadOnly"

app = Flask(__name__, instance_relative_config=True)
# additional env variables prefixed with `FLASK_`
app.config.from_prefixed_env()

# Instantiate shared gremlin query client with connection pool-size of 4
gremlin_client = Client(
    "ws://localhost:8182/gremlin", READ_ONLY_TRAVERSAL_SOURCE, pool_size=4
)

gremlin_remote_connection = DriverRemoteConnection(
    "ws://localhost:8182/gremlin",
    READ_ONLY_TRAVERSAL_SOURCE,
    message_serializer=serializer.GraphSONMessageSerializer(),
)


@dataclass
class GremlinRequest:
    query: str


def attemptParseFromJson(cls, json_dict):
    try:
        return cls(**json_dict)
    except TypeError:
        return None


@app.route("/packages", methods=["POST"])
def packages():
    if request.json is None:
        return (
            {"error": "Request did not include a JSON payload"},
            400,
        )
    try:
        packages_request: ListPackagesRequest = ListPackagesRequestSchema().load(
            request.json
        )  # type: ignore
    except ValidationError as e:
        return (
            {
                "error": "Request body did not have the expected schema. "
                + f"Errors: {e.messages}"
            },
            400,
        )
    response = list_packages(packages_request, gremlin_remote_connection)
    return jsonify(ListPackagesResponseSchema().dump(response))


@app.route("/gremlin", methods=["POST"])
def gremlin():
    try:
        # FIXME: Use a marshmallow Schema here
        payload = attemptParseFromJson(GremlinRequest, request.json)
        if payload is None:
            return (
                {"error": "Request JSON payload did not have expected schema."},
                400,
            )
        GR = query.GremlinResult(gremlin_client, payload.query, clean_gremlin=True)
        result = GR.to_dict()
        match result:
            case None:
                raise Exception("Could not get Gremlin result from server.")
            case _:
                return jsonify(result)
    except Exception as e:
        app.logger.exception(e)
        # FIXME: Use a marshmallow Schema here as well
        return jsonify({"error": str(e)}), 500

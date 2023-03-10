"""Initialize Flask app."""
from flask import Flask, request, jsonify
from explorer.queries import query
from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.driver.client import Client
from dataclasses import dataclass

READ_ONLY_TRAVERSAL_SOURCE = "gReadOnly"

app = Flask(__name__, instance_relative_config=True)
# the defaults in config.py
app.config.from_object("config.Default")
# additional env variables prefixed with `FLASK_`
app.config.from_prefixed_env()

# Instantiate shared gremlin query client with connection pool-size of 4
gremlin_client = Client(
    "ws://localhost:8182/gremlin", READ_ONLY_TRAVERSAL_SOURCE, pool_size=4
)

gremlin_remote_connection = DriverRemoteConnection(
    "ws://localhost:8182/gremlin", READ_ONLY_TRAVERSAL_SOURCE
)


@dataclass
class GremlinRequest:
    query: str


def attemptParseFromJson(cls, json_dict):
    try:
        return cls(**json_dict)
    except TypeError:
        return None


@app.route("/packages", methods=["GET"])
def packages():
    name_field = "pname"
    g = traversal().withRemote(gremlin_remote_connection)
    # Query all node properties
    # FIXME: This loads the entire graph into memory. We should probably be paginating here.
    query_result = g.V().project(name_field).by(name_field).toList()
    package_names = []
    for r in query_result:
        if name_field in r:
            p = r[name_field]
            if p is not None:
                package_names.append(p)
    return jsonify({"packages": [{"name": pn} for pn in package_names]})


@app.route("/gremlin", methods=["POST"])
def gremlin():
    try:
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
        return jsonify({"error": str(e)}), 500

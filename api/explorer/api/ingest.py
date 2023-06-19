import logging
from contextlib import closing
from typing import IO

import click
from explorer.core import model
from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.process.graph_traversal import GraphTraversalSource
from pydantic import ValidationError

from explorer.api import graph
from explorer.api.gremlin import default_remote_connection

logger = logging.getLogger(__name__)


class IngestionError(Exception):
    """Exception for errors related to ingesting data"""

    def __init__(
        self, *args: object, derivation: model.Derivation | None = None
    ) -> None:
        self.derivation = derivation
        super().__init__(*args)


def core_to_graph(derivation: model.Derivation) -> list[graph.Derivation]:
    """
    Convert a derivation from the core (extraction) to the graph (storage) model.
    Turn a multi-outputs derivation into many nodes.

    Raises:
        IngestionError: If the input derivation could not be parsed into an API
            Derivation, for example due to missing required fields.
    """
    if len(derivation.outputs) > 0:
        if any(output.output_path is None for output in derivation.outputs):
            raise IngestionError(
                "Empty output path for one of the derivation outputs",
                derivation=derivation,
            )
        return [
            graph.Derivation(
                output_path=output.output_path,
                attribute_path=derivation.attribute_path + "." + output.name,
            )
            for output in derivation.outputs
        ]
    else:
        if derivation.output_path is None:
            raise IngestionError("Empty output path", derivation=derivation)
        return [
            graph.Derivation(
                output_path=derivation.output_path,
                attribute_path=derivation.attribute_path,
            )
        ]


def _edge_from_build_input_type(build_input_type: model.BuildInputType) -> graph.Edge:
    """Creates an edge object based on the given build input type."""
    if build_input_type == model.BuildInputType.BUILD_INPUT.value:
        return graph.HasBuildInput()
    elif build_input_type == model.BuildInputType.PROPAGATED_BUILD_INPUT.value:
        return graph.HasPropagatedBuildInput()
    elif build_input_type == model.BuildInputType.NATIVE_BUILD_INPUT.value:
        return graph.HasNativeBuiltInput()
    else:
        raise IngestionError(
            "The provided BuildInputType does not correspond to a known edge type:"
            f" {build_input_type}"
        )


def ingest_derivations(
    infile: IO[str],
    g: GraphTraversalSource,
) -> None:
    """
    Writes the entire provided Nix graph to the provided Gremlin traversal source.
    """
    for line in infile:
        try:
            derivation = model.Derivation.parse_raw(line)
        except ValidationError as e:
            click.echo(e, err=True)
            continue

        # convert from extraction to db model
        # possibly handling multi-outputs derivation
        derivation_nodes = core_to_graph(derivation)
        build_input_nodes = [
            graph.Derivation(
                output_path=build_input.output_path,
                attribute_path=None,
            )
            for build_input in derivation.build_inputs
            if build_input.output_path is not None
        ]

        # share same traversal to run all operations at once
        traversal = g.get_graph_traversal()

        # insert nodes
        for node in derivation_nodes:
            # insert if it does not exist, otherwise update properties
            traversal = graph.upsert_unique_vertex(traversal, node)
        for node in build_input_nodes:
            # insert build input nodes to allow creating the edge
            traversal = graph.insert_unique_vertex(traversal, node)
        # Now that the traversal has been constructed, let's evaluate it

        # insert edges
        for derivation_node in derivation_nodes:
            for build_input, build_input_node in zip(
                derivation.build_inputs,
                build_input_nodes,
            ):
                edge: graph.Edge = _edge_from_build_input_type(
                    build_input.build_input_type
                )
                traversal = graph.insert_unique_directed_edge(
                    g=traversal,
                    edge=edge,
                    from_vertex=derivation_node,
                    to_vertex=build_input_node,
                )

        traversal.iterate()
        logger.info("%s", derivation.output_path)


@click.command(context_settings={"show_default": True})
@click.argument(
    "infile",
    required=True,
    type=click.File(),
)
@click.option(
    "--gremlin-server",
    default="ws://localhost:8182/gremlin",
    help="The full URL to use to connect to the target Gremlin Server",
)
@click.option(
    "--gremlin-source",
    default="g",
    help=(
        "The name of the traversal source to use when communicating with Gremlin"
        " Server. This value is expected to already be configured in the remote Gremlin"
        " Server."
    ),
)
@click.option(
    "--verbose",
    is_flag=True,
)
def main(
    infile: IO[str],
    gremlin_server: str,
    gremlin_source: str,
    verbose: bool,
):
    """
    Ingests from INFILE to a Gremlin Server.

    INFILE should be a JSONL stream of derivations as defined by `model.Derivation`.
    """
    logging.basicConfig(level=logging.DEBUG if verbose else logging.INFO)
    logging.getLogger("websockets.client").setLevel(logging.WARNING)
    logging.getLogger("gremlinpython").setLevel(
        logging.INFO if verbose else logging.WARNING
    )
    with closing(
        default_remote_connection(
            gremlin_server,
            traversal_source=gremlin_source,
        ),
    ) as remote:
        g = traversal().with_remote(remote)
        ingest_derivations(infile, g=g)


if __name__ == "__main__":
    main()

from contextlib import closing
import logging
from typing import Callable, Iterable, TypeVar
import click
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.driver import serializer
from gremlin_python.process.graph_traversal import GraphTraversalSource

from explorer.core import model
from explorer.api import graph

logger = logging.getLogger(__name__)


class IngestionError(Exception):
    """Exception for errors related to ingesting data"""

    pass


def core_to_graph_model(pkg: model.Package) -> list[graph.Package]:
    if pkg.nixpkgs_metadata is None:
        raise IngestionError(
            "The provided package did not have its nixpkgs_metadata attribute set, but"
            " this is required for converting it to the schema used by the"
            f" nixpkgs-graph-explorer graph. Package: {pkg}"
        )
    if pkg.nixpkgs_metadata.pname is None:
        raise IngestionError(
            "The provided package did not have its nixpkgs_metadata.pname attribute"
            " set, but this is required for converting it to the schema used by the"
            f" nixpkgs-graph-explorer graph. Package: {pkg}"
        )
    return [
        graph.Package(pname=pkg.nixpkgs_metadata.pname, outputPath=op)
        for op in pkg.output_paths.values()
    ]


def safe_parse_package(model_pkg: model.Package) -> list[graph.Package]:
    try:
        pkgs = core_to_graph_model(model_pkg)
    except IngestionError as e:
        logger.exception(e)
        pkgs = []
    return pkgs


# TODO: Add retry logic
def _do_next_graph_level(
    model_pkg: model.Package,
    do_fn: Callable[[graph.Package, graph.Package], None],
) -> list[model.Package]:
    # Map package from core model in that used in the graph. This will expand the
    # input node into N nodes based on the number of the node's output paths.
    pkgs = safe_parse_package(model_pkg)
    # Create all dependency nodes and draw an edge between the input package nodes and
    # them.
    for pkg in pkgs:
        for build_input in model_pkg.build_inputs:
            try:
                # Since build_inputs is defined using the core model, we need to parse
                # it into the graph model similar to what we do above for model_pkg.
                build_input_pkgs = safe_parse_package(build_input.package)
            except IngestionError as e:
                logger.exception(e)
                build_input_pkgs = []
            for bi_pkg in build_input_pkgs:
                do_fn(pkg, bi_pkg)
    return [bi.package for bi in model_pkg.build_inputs]


A = TypeVar("A")


def flatten(lls: Iterable[Iterable[A]]) -> list[A]:
    return [x for sublist in lls for x in sublist]


def traverse(
    nix_graph: model.NixGraph,
    root_fn: Callable[[graph.Package], None],
    do_fn: Callable[[graph.Package, graph.Package], None],
):
    for model_pkg in nix_graph.packages:
        # Apply the user provided function to the first layer
        pkgs = safe_parse_package(model_pkg)
        for pkg in pkgs:
            root_fn(pkg)
        next_pkgs = _do_next_graph_level(model_pkg, do_fn)
        # Iterate over each remaining layer until no more remain
        while next_pkgs:
            # TODO: Exception handling
            next_pkgs = flatten(
                map(lambda p: _do_next_graph_level(p, do_fn), next_pkgs)
            )


def ingest_nix_graph(nix_graph: model.NixGraph, g: GraphTraversalSource) -> None:
    def write_root_node_to_graph(root_package: graph.Package) -> None:
        graph.insert_unique_vertex(root_package, g)

    def write_layer_to_graph(
        current_package: graph.Package, upstream_package: graph.Package
    ) -> None:
        graph.insert_unique_vertex(upstream_package, g)
        graph.insert_unique_directed_edge(
            graph.DependsOn(),
            from_vertex=current_package,
            to_vertex=upstream_package,
            g=g,
        )

    traverse(nix_graph, write_root_node_to_graph, write_layer_to_graph)


@click.command(
    help="Ingests a NixGraph JSON file to the specified Gremlin Server",
    context_settings={"show_default": True},
)
@click.option(
    "--graph-json",
    required=True,
    help="The JSON file containing the NixGraph to ingest",
    type=click.Path(exists=True, dir_okay=False),
)
@click.option(
    "--gremlin-server",
    default="ws://localhost:8182/gremlin",
    help="The full URL to use to connect to the target Gremlin Server",
)
@click.option(
    "--gremlin-source",
    default="g",
    help="The name of the traversal source to use when communicating with Gremlin Server. This value is expected to already be configured in the remote Gremlin Server.",
)
def main(graph_json: str, gremlin_server: str, gremlin_source: str):

    click.echo(f"Attempting to read Nix graph from {graph_json}...")
    nix_graph = model.NixGraph.parse_file(graph_json)
    click.echo("Done.")

    click.echo(f"Configuring connection to Gremlin Server...")
    with closing(
        DriverRemoteConnection(
            gremlin_server,
            gremlin_source,
            message_serializer=serializer.GraphSONMessageSerializer(),
        )
    ) as remote:
        g = traversal().with_remote(remote)
        click.echo("Done.")

        click.echo("Beginning to write Nix graph to Gremlin Server...")
        ingest_nix_graph(nix_graph, g)
        click.echo("Success! Exiting...")


if __name__ == "__main__":
    main()

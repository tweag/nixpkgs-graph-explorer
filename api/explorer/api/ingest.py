import logging
from typing import Callable, Iterable, TypeVar
import click
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.driver import serializer

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
    do_fn: Callable[[graph.Package, graph.Package | None], None],
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
    do_fn: Callable[[graph.Package, graph.Package | None], None],
):
    for model_pkg in nix_graph.packages:
        # Apply the user provided function to the first layer
        pkgs = safe_parse_package(model_pkg)
        for pkg in pkgs:
            do_fn(pkg, None)
        next_pkgs = _do_next_graph_level(model_pkg, do_fn)
        # Iterate over each remaining layer until no more remain
        while next_pkgs:
            # TODO: Exception handling
            next_pkgs = flatten(
                map(lambda p: _do_next_graph_level(p, do_fn), next_pkgs)
            )


@click.command()
@click.option(
    "--graph_json",
    required=True,
    help="The JSON file containing the Nix graph to ingest",
    type=click.Path(exists=True, dir_okay=False),
)
def main(graph_json: str):
    from explorer.core.model import (
        NixGraph,
        NixpkgsMetadata,
        Package,
        OutputPathName,
        BuildInput,
        BuildInputType,
    )

    click.echo(f"Attempting to read Nix graph from {graph_json}...")
    nix_graph = NixGraph.parse_file(graph_json)
    click.echo("Done.")

    click.echo(f"Configuring connection to Gremlin Server...")
    conn = DriverRemoteConnection(
        "ws://localhost:8182/gremlin",
        "g",
        message_serializer=serializer.GraphSONMessageSerializer(),
    )
    g = traversal().with_remote(conn)
    click.echo("Done.")

    # vertex_count = 0 # Variables for future printing...
    # edge_count = 0

    def write_layer_to_graph(
        current_package: graph.Package, upstream_package: graph.Package | None
    ) -> None:
        if upstream_package is None:
            graph.insert_unique_vertex(current_package, g)
            return
        graph.insert_unique_vertex(upstream_package, g)
        graph.insert_unique_directed_edge(
            graph.DependsOn(),
            from_vertex=current_package,
            to_vertex=upstream_package,
            g=g,
        )

    click.echo("Beginning to write Nix graph to Gremlin Server...")
    traverse(nix_graph, write_layer_to_graph)
    click.echo("Success! Exiting...")


if __name__ == "__main__":
    main()

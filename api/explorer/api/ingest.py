import logging
from contextlib import closing
from typing import Callable, Iterable, TypeVar

import click
from explorer.core import model
from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.process.graph_traversal import GraphTraversalSource

from explorer.api import graph
from explorer.api.gremlin import default_remote_connection

logger = logging.getLogger(__name__)


class IngestionError(Exception):
    """Exception for errors related to ingesting data"""

    def __init__(self, *args: object, core_pkg: model.Package | None = None) -> None:
        self.core_pkg = core_pkg
        super().__init__(*args)


def core_to_graph_model(pkg: model.Package) -> list[graph.Package]:
    """
    Attempts to convert core NixGraph package data to the API Package data using its
    output path

    This is a 1 to many operation since the input package may have multiple output
    paths.

    Args:
        pkg (model.Package): The input core package

    Raises:
        IngestionError: If the input package could not be parsed into an API Package,
            for example due to missing required fields.

    Returns:
        list[graph.Package]
    """
    if pkg.nixpkgs_metadata is None:
        raise IngestionError(
            (
                "The provided package did not have its nixpkgs_metadata attribute set,"
                " but this is required for converting it to the schema used by the"
                f" nixpkgs-graph-explorer graph. Package: {pkg}"
            ),
            core_pkg=pkg,
        )
    if pkg.nixpkgs_metadata.pname is None:
        raise IngestionError(
            (
                "The provided package did not have its nixpkgs_metadata.pname attribute"
                " set, but this is required for converting it to the schema used by the"
                f" nixpkgs-graph-explorer graph. Package: {pkg}"
            ),
            core_pkg=pkg,
        )
    return [
        graph.Package(pname=pkg.nixpkgs_metadata.pname, outputPath=op.path)
        for op in pkg.output_paths
    ]


def safe_parse_package(model_pkg: model.Package) -> list[graph.Package]:
    """Safely parses a core NixGraph package into an API Package.

    In cases where exceptions are raised, they will be logged and an empty list
    will be returned.

    Args:
        model_pkg (model.Package): The input core package

    Returns:
        list[graph.Package]: All graph packages which could be parsed from the
            input core package.
    """
    try:
        pkgs = core_to_graph_model(model_pkg)
    except IngestionError as e:
        logger.exception(e)
        pkgs = []
    return pkgs


def _edge_from_build_input_type(build_input_type: model.BuildInputType) -> graph.Edge:
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


# TODO: Add retry logic
def _do_next_graph_level(
    model_pkg: model.Package,
    do_fn: Callable[[graph.Package, graph.Package, graph.Edge], None],
) -> list[model.Package]:
    # Map package from core model in that used in the graph. This will expand the
    # input node into N nodes based on the number of the node's output paths.
    pkgs = safe_parse_package(model_pkg)
    # Create all dependency nodes and draw an edge between the input package nodes and
    # them.
    for pkg in pkgs:
        for build_input in model_pkg.build_inputs:
            edge = _edge_from_build_input_type(build_input.build_input_type)
            try:
                # Since build_inputs is defined using the core model, we need to parse
                # it into the graph model similar to what we do above for model_pkg.
                build_input_pkgs = safe_parse_package(build_input.package)
            except IngestionError as e:
                logger.exception(e)
                build_input_pkgs = []
            for bi_pkg in build_input_pkgs:
                do_fn(pkg, bi_pkg, edge)
    return [bi.package for bi in model_pkg.build_inputs]


A = TypeVar("A")


def flatten(lls: Iterable[Iterable[A]]) -> list[A]:
    """Flattens nested iterables into a list"""
    return [x for sublist in lls for x in sublist]


def traverse(
    nix_graph: model.NixGraph,
    root_fn: Callable[[graph.Package], None],
    do_fn: Callable[[graph.Package, graph.Package, graph.Edge], None],
):
    """Traverse a Nix graph

    Performs a breadth-first traversal of a nix graph, executing the provided
    functions on each level of the graph.

    Args:
        nix_graph (model.NixGraph): The NixGraph to traverse
        root_fn (Callable[[graph.Package], None]): A function to call only on the root
            level of the nix graph. This can be useful for things like initialization
            steps.
        do_fn (Callable[[graph.Package, graph.Package, graph.Edge], None]): A function
            to call on every level of the graph, including the root level. This
            function is expected to take three arguments which correspond to the
            current level, the next level, and the edge describing the relationship
            between the current level and the next level.
    """
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
    """Writes the entire provided Nix graph to the provided Gremlin traversal source

    Args:
        nix_graph (model.NixGraph): The Nix graph to ingest
        g (GraphTraversalSource): The graph traversal source to use for ingesting
            the Nix graph
    """

    # Note: As an optimization we split logic for writing the graph into two
    # functions. The first one ensures that every root node of the graph exists
    def write_root_node_to_graph(root_package: graph.Package) -> None:
        graph.insert_unique_vertex(root_package, g)

    # Then, this second function handles writing the next level of the graph as well
    # as the edge connecting the levels, assuming that the current level already exists
    # in the Gremlin graph.
    def write_layer_to_graph(
        current_package: graph.Package,
        upstream_package: graph.Package,
        edge: graph.Edge,
    ) -> None:
        graph.insert_unique_vertex(upstream_package, g)
        graph.insert_unique_directed_edge(
            edge,
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
    help=(
        "The name of the traversal source to use when communicating with Gremlin"
        " Server. This value is expected to already be configured in the remote Gremlin"
        " Server."
    ),
)
def main(graph_json: str, gremlin_server: str, gremlin_source: str):
    click.echo(f"Attempting to read Nix graph from {graph_json}...")
    nix_graph = model.NixGraph.parse_file(graph_json)
    click.echo("Done.")

    click.echo("Configuring connection to Gremlin Server...")
    with closing(
        default_remote_connection(gremlin_server, traversal_source=gremlin_source)
    ) as remote:
        g = traversal().with_remote(remote)
        click.echo("Done.")

        click.echo("Beginning to write Nix graph to Gremlin Server...")
        ingest_nix_graph(nix_graph, g)
        click.echo("Success! Exiting...")


if __name__ == "__main__":
    main()

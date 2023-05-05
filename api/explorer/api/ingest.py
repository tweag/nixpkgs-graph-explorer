import builtins
import logging
from contextlib import closing

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


def search_package_by_path(
    nix_graph: model.NixGraph, search_path: str
) -> list[model.Package]:
    """
    Returns a list of Package objects where at least one of the path in the output_paths
    field matches the search_string.
    """
    return [
        package
        for package in nix_graph.packages
        if builtins.any(
            search_path == output_path.path for output_path in package.output_paths
        )
    ]


def ingest_nix_graph(nix_graph: model.NixGraph, g: GraphTraversalSource) -> None:
    """Writes the entire provided Nix graph to the provided Gremlin traversal source

    Args:
        nix_graph (model.NixGraph): The Nix graph to ingest
        g (GraphTraversalSource): The graph traversal source to use for ingesting
            the Nix graph
    """

    # Note: As an optimization we split logic for writing the graph into two
    # functions. The first one ensures that every node of the graph exists
    def write_node_to_graph(package: graph.Package) -> None:
        graph.insert_unique_vertex(package, g)

    # Then, this second function handles writing the edges of the graph
    def write_edge_to_graph(
        current_package: graph.Package,
        dependency_package: graph.Package,
        edge: graph.Edge,
    ) -> None:
        graph.insert_unique_directed_edge(
            edge,
            from_vertex=current_package,
            to_vertex=dependency_package,
            g=g,
        )

    # Write nodes to the Gremlin graph
    click.echo("Writing nodes to the Gremlin graph...")
    with click.progressbar(nix_graph.packages) as model_pkgs_with_bar:
        for model_package in model_pkgs_with_bar:
            pkgs = safe_parse_package(model_package)
            for pkg in pkgs:
                write_node_to_graph(pkg)

    # Write edges to the Gremlin graph
    click.echo("Writing edges to the Gremlin graph...")
    with click.progressbar(nix_graph.packages) as model_pkgs_with_bar:
        for model_package in model_pkgs_with_bar:
            pkgs = safe_parse_package(model_package)
            for build_input in model_package.build_inputs:
                edge = _edge_from_build_input_type(build_input.build_input_type)
                bi_model_packages = search_package_by_path(
                    nix_graph, build_input.output_path
                )
                for bi_model_package in bi_model_packages:
                    bi_graph_pkgs = safe_parse_package(bi_model_package)
                    for bi_graph_package in bi_graph_pkgs:
                        for current_graph_package in pkgs:
                            write_edge_to_graph(
                                current_graph_package, bi_graph_package, edge
                            )


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

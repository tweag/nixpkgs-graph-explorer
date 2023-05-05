import logging
from contextlib import closing
from typing import Callable, Tuple
import threading

from concurrent.futures import ALL_COMPLETED, ThreadPoolExecutor, wait
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
    filtered_package = filter(
        lambda package: search_path in [p.path for p in package.output_paths],
        nix_graph.packages,
    )
    return list(filtered_package)


def split_nix_graph(nix_graph: model.NixGraph):
    packages = {}
    edge_refs = {}
    edges = []
    for p in nix_graph.packages:
        parsed = safe_parse_package(p)
        for ps in parsed:
            packages[ps.outputPath] = ps
            edge_refs |= {
                (
                    ps.outputPath,
                    build_input.output_path,
                ): _edge_from_build_input_type(build_input.build_input_type)
                for build_input in p.build_inputs
            }
    missing = []
    for paths, edge in edge_refs.items():
        package_output_path, dependency_output_path = paths
        if package_output_path not in packages:
            raise Exception("EXPLODE")
        if dependency_output_path not in packages:
            missing.append(dependency_output_path)
            continue
        edges.append(
            (packages[package_output_path], edge, packages[dependency_output_path])
        )

    if missing:
        print(f"Missing packages for a total of {len(missing)} build inputs")
    return list(packages.values()), edges


def ingest_nix_graph(
    nix_graph: model.NixGraph, g_factory: Callable[[], GraphTraversalSource]
) -> None:
    """Writes the entire provided Nix graph to the provided Gremlin traversal source

    Args:
        nix_graph (model.NixGraph): The Nix graph to ingest
        g (GraphTraversalSource): The graph traversal source to use for ingesting
            the Nix graph
    """

    packages, edges = split_nix_graph(nix_graph)

    # Write nodes to the Gremlin graph
    click.echo("Writing nodes to the Gremlin graph...")

    def get_local_client() -> GraphTraversalSource:
        thread_local = threading.local()
        g = getattr(thread_local, "g", None)
        if g is None:
            thread_local.g = g_factory()
        return thread_local.g

    def write_package(p: graph.Package):
        # print(p)
        g = get_local_client()
        graph.insert_unique_vertex(p, g)

    def write_edge(input: Tuple[graph.Package, graph.Edge, graph.Package]):
        current_package, edge, dependency_package = input
        g = get_local_client()
        graph.insert_unique_directed_edge(
            edge,
            from_vertex=current_package,
            to_vertex=dependency_package,
            g=g,
        )

    with ThreadPoolExecutor() as ex:
        click.echo("Writing packages")
        with click.progressbar(length=len(packages)) as bar:
            futures = [ex.submit(write_package, p) for p in packages]
            for f in futures:
                f.add_done_callback(lambda _x: bar.update(1))
            wait(futures)
            # FIXME: Check futures for exceptions

        click.echo("Writing edges")
        with click.progressbar(length=len(edges)) as bar:
            futures = [ex.submit(write_edge, e) for e in edges]
            for f in futures:
                f.add_done_callback(lambda _x: bar.update(1))
            wait(futures)
            # FIXME: Check futures for exceptions


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
        g_factory = lambda: traversal().with_remote(remote)
        click.echo("Done.")

        click.echo("Purging existing entities.")
        g_factory().V().drop().iterate().to_list()
        g_factory().E().drop().iterate().to_list()
        click.echo("Done.")

        click.echo("Beginning to write Nix graph to Gremlin Server...")
        ingest_nix_graph(nix_graph, g_factory)
        click.echo("Success! Exiting...")


if __name__ == "__main__":
    main()

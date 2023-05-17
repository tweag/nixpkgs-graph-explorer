import dataclasses
import logging
import threading
import time
from concurrent.futures import ThreadPoolExecutor, wait
from contextlib import closing
from typing import Callable, Optional, Tuple

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
    if pkg.parsed_name is None:
        raise IngestionError(
            (
                "The provided package did not have its parsed_name attribute set,"
                " but this is required for converting it to the schema used by the"
                f" nixpkgs-graph-explorer graph. Package: {pkg}"
            ),
            core_pkg=pkg,
        )
    if pkg.parsed_name.name is None:
        raise IngestionError(
            (
                "The provided package did not have its parsed_name.name attribute"
                " set, but this is required for converting it to the schema used by the"
                f" nixpkgs-graph-explorer graph. Package: {pkg}"
            ),
            core_pkg=pkg,
        )
    return [
        graph.Package(pname=pkg.parsed_name.name, outputPath=op.path)
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


@dataclasses.dataclass(frozen=True)
class _IngestionContext:
    """Class holding data that is passed along successive calls to
    split_nix_package"""

    """Map from package output path to graph Package object.
       Continuously grows while ingesting"""
    packages: dict[str, graph.Package] = dataclasses.field(default_factory=dict)
    """Edges that could not be added yet, because the package
       that is the destination of the edge has not been processed yet.
       For example if package A at "/A" depends on "/B" and package "A" is
       processed first, this dict will be {"/B": [(A, edge)]} until B is processed.
       When package B is processed, "A -edge-> B" will be returned in the
       ingestion methods below, and the dict is shrunk.

       This dict has the destination of the edge as key, to allow for fast
       lookup (as opposed to using a list of triplets)."""
    # TODO, this is a multimap, is there a Python class for that?
    pending_edges: dict[
        str, list[tuple[graph.Package, graph.Edge]]
    ] = dataclasses.field(default_factory=dict)


def split_nix_package(
    nix_package: model.Package, ctxt: _IngestionContext
) -> tuple[list[graph.Package], list[tuple[graph.Package, graph.Edge, graph.Package]]]:
    """
    Processes nix_package, returning the new edges that must be created,
    and augmenting ctxt with both the new packages (ctxt.packages) and
    the edges to add later on (ctxt.pending_edges)

    Args:
        nix_package (model.Package): parsed Nix package (core model)
        ctxt (_IngestionContext): accumulator of the ingestion
    Returns:
        tuple[
            list[graph.Package],
            list[tuple[graph.Package, graph.Edge, graph.Package]]:
        ]
        The packages and edges created while processing nix_package. In the edges,
        a tuple (A, edge, B) means A -edge-> B.
    """
    edges: list[tuple[graph.Package, graph.Edge, graph.Package]] = []

    parsed_pkgs: list[graph.Package] = safe_parse_package(nix_package)
    for pkg in parsed_pkgs:
        ctxt.packages[pkg.outputPath] = pkg

        # Go over the build inputs of the package being processed, and
        # record edges to these build inputs.
        for build_input in nix_package.build_inputs:
            edge: graph.Edge = _edge_from_build_input_type(build_input.build_input_type)
            dest_output_path: str = build_input.output_path
            dest: Optional[graph.Package] = ctxt.packages.get(dest_output_path, None)
            if dest:
                # Record edge right away
                edges.append((pkg, edge, dest))
            else:
                # Register for being recorded when destination will be processed
                if dest_output_path in ctxt.pending_edges:
                    ctxt.pending_edges[dest_output_path].append((pkg, edge))
                else:
                    ctxt.pending_edges[dest_output_path] = [(pkg, edge)]

    # For every new package, add pending edges to this package
    for pkg in parsed_pkgs:
        if pkg.outputPath not in ctxt.pending_edges:
            # No edge to this package was pending addition
            continue
        src_edge_pairs = ctxt.pending_edges[pkg.outputPath]
        for source_pkg, edge in src_edge_pairs:
            edges.append((source_pkg, edge, pkg))
        # Shrink pending_edges, as key has been processed
        ctxt.pending_edges.pop(pkg.outputPath)

    return parsed_pkgs, edges


def ingest_nix_package(
    nix_package: model.Package,
    ctxt: _IngestionContext,
    g_factory: Callable[[], GraphTraversalSource],
) -> None:
    """Writes the entire provided Nix graph to the provided Gremlin traversal source

    Args:
        nix_package (model.NixPackage): The Nix package to ingest
        ctxt (_IngestionContext): The ingestion context
        g (GraphTraversalSource): The graph traversal source to use for ingesting
            the Nix graph
    """

    packages, edges = split_nix_package(nix_package, ctxt)

    retries = 3  # Number of retry attempts

    # Write nodes to the Gremlin graph
    click.echo("Writing nodes to the Gremlin graph...")

    def get_local_client() -> GraphTraversalSource:
        thread_local = threading.local()
        g = getattr(thread_local, "g", None)
        if g is None:
            thread_local.g = g_factory()
        return thread_local.g

    def write_package(p: graph.Package):
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
        failed_packages = packages.copy()
        failed_edges = edges.copy()

        # Ingest vertices
        for attempt in range(1, retries + 1):
            click.echo(f"Writing packages (Attempt {attempt})")

            with click.progressbar(length=len(failed_packages)) as bar:
                futures = [ex.submit(write_package, p) for p in failed_packages]
                for f in futures:
                    f.add_done_callback(lambda _x: bar.update(1))
                wait(futures)
                failed_packages_copy = failed_packages.copy()
                for i, f in enumerate(futures):
                    if f.exception():
                        logger.exception(
                            "Error occurred during package ingestion"
                            " (Attempt %s, Vertex: %s): %s",
                            attempt,
                            failed_packages[i],
                            str(f.exception()),
                        )
                    else:
                        # Remove the vertex as successfully ingested
                        failed_packages.remove(failed_packages_copy[i])
            if failed_packages == []:
                # All vertices ingested successfully, break the loop
                break
            if attempt < retries:
                click.echo("Retrying failed package ingestion in 5 seconds...")
                time.sleep(5)
            else:
                click.echo(
                    "Maximum number of retries reached for package ingestion. Aborting."
                )
                # Abort ingestion if maximum retries reached
                break

        # Ingest edges
        for attempt in range(1, retries + 1):
            click.echo(f"Writing edges (Attempt {attempt})")
            with click.progressbar(length=len(failed_edges)) as bar:
                futures = [ex.submit(write_edge, e) for e in failed_edges]
                for f in futures:
                    f.add_done_callback(lambda _x: bar.update(1))
                wait(futures)
                failed_edges_copy = failed_edges.copy()
                for i, f in enumerate(futures):
                    if f.exception():
                        logger.exception(
                            "Error occurred during edge ingestion "
                            "(Attempt %s, Edge: %s): %s",
                            attempt,
                            failed_edges[i],
                            str(f.exception()),
                        )
                    else:
                        # Remove the edge as successfully ingested
                        failed_edges.remove(failed_edges_copy[i])
            if failed_edges == []:
                # All edges ingested successfully, break the loop
                break

            if attempt < retries:
                click.echo("Retrying failed edge ingestion in 5 seconds...")
                time.sleep(5)
            else:
                click.echo(
                    "Maximum number of retries reached for edge ingestion. Aborting."
                )
                # Abort ingestion if maximum retries reached
                break


def ingest_nix_graph(
    nix_graph: model.NixGraph, g_factory: Callable[[], GraphTraversalSource]
) -> None:
    """Writes the entire provided Nix graph to the provided Gremlin traversal source

    Args:
        nix_graph (model.NixGraph): The Nix graph to ingest
        g (GraphTraversalSource): The graph traversal source to use for ingesting
            the Nix graph
    """
    ctxt = _IngestionContext()
    for package in nix_graph.packages:
        ingest_nix_package(package, ctxt, g_factory)

    nb_missing_edges = sum(
        len(src_edge_pairs) for src_edge_pairs in ctxt.pending_edges.values()
    )
    if nb_missing_edges > 0:
        print(
            f"Warning: {nb_missing_edges} build input(s) in the input JSON file "
            "do not have a corresponding package and will be ignored."
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

        def g_factory():
            return traversal().with_remote(remote)

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

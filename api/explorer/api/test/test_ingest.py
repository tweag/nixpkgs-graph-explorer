import pytest
from explorer.core import model
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.process.anonymous_traversal import traversal

from explorer.api import graph
from explorer.api.gremlin import default_remote_connection

#######################################################################################
# Mock Data
#######################################################################################

dummy_pkg_d = model.Package(
    name="D",
    output_paths=[model.OutputPath(name="out", path="/D")],
    nixpkgs_metadata=model.NixpkgsMetadata(
        pname="D", version="1.0", broken=False, license="MIT"
    ),
    build_inputs=[],
)


dummy_pkg_c = model.Package(
    name="C",
    output_paths=[model.OutputPath(name="out", path="/C")],
    nixpkgs_metadata=model.NixpkgsMetadata(
        pname="C",
        version="1.0",
        broken=False,
        license="MIT",
    ),
    build_inputs=[],
)

dummy_pkg_b = model.Package(
    name="B",
    output_paths=[model.OutputPath(name="out", path="/B")],
    nixpkgs_metadata=model.NixpkgsMetadata(
        pname="B", version="1.0", broken=False, license="MIT"
    ),
    build_inputs=[
        model.BuildInput(
            build_input_type=model.BuildInputType.BUILD_INPUT,
            package=dummy_pkg_c,
        )
    ],
)

dummy_pkg_a = model.Package(
    name="A",
    output_paths=[model.OutputPath(name="out", path="/A")],
    nixpkgs_metadata=model.NixpkgsMetadata(
        pname="A", version="1.0", broken=False, license="MIT"
    ),
    build_inputs=[
        model.BuildInput(
            build_input_type=model.BuildInputType.BUILD_INPUT,
            package=dummy_pkg_b,
        ),
        model.BuildInput(
            build_input_type=model.BuildInputType.BUILD_INPUT,
            package=dummy_pkg_d,
        ),
    ],
)

dummy_nix_graph = model.NixGraph(packages=[dummy_pkg_a])


#######################################################################################
# Fixture(s)
#######################################################################################
@pytest.fixture
def graph_connection():
    # FIXME: use a traversal_source purely dedicated to tests
    conn = default_remote_connection(
        "ws://localhost:8182/gremlin", traversal_source="g"
    )
    g = traversal().with_remote(conn)
    yield conn
    # Dropping all data from graph on teardown
    g.V().drop().iterate()
    g.E().drop().iterate()
    conn.close()


#######################################################################################
# Tests
#######################################################################################


def test_unit_core_to_graph_model():
    """Unit test for mapping core packages to graph packages"""
    from explorer.api.ingest import core_to_graph_model

    pkg = model.Package(
        name="foo",
        output_paths=[
            model.OutputPath(name="dev", path="/foo/dev"),
            model.OutputPath(name="lib", path="/foo/lib"),
        ],
        nixpkgs_metadata=model.NixpkgsMetadata(
            pname="foo", version="1.0", broken=False, license="MIT"
        ),
        build_inputs=[],
    )
    converted_pkgs = core_to_graph_model(pkg)

    expected_pkg_dev = graph.Package(pname="foo", outputPath="/foo/dev")
    expected_pkg_lib = graph.Package(pname="foo", outputPath="/foo/lib")
    assert expected_pkg_dev in converted_pkgs
    assert expected_pkg_lib in converted_pkgs


def test_unit_core_to_graph_model_no_output_path():
    """Unit test for mapping core packages with no output path to graph packages"""
    from explorer.api.ingest import core_to_graph_model

    pkg = model.Package(
        name="foo",
        output_paths=[],
        nixpkgs_metadata=model.NixpkgsMetadata(
            pname="foo", version="1.0", broken=False, license="MIT"
        ),
        build_inputs=[],
    )
    assert core_to_graph_model(pkg) == []


def test_unit_core_to_graph_model_no_metadata_raises():
    """Check that attempting to parse a package without nixpkgs_metadata fails"""
    from explorer.api.ingest import IngestionError, core_to_graph_model

    pkg = model.Package(
        name="foo",
        output_paths=[],
        nixpkgs_metadata=None,
        build_inputs=[],
    )
    with pytest.raises(IngestionError):
        core_to_graph_model(pkg)


def test_unit_core_to_graph_model_no_pname_raises():
    """Check that attempting to parse a package without a pname fails"""
    from explorer.api.ingest import IngestionError, core_to_graph_model

    pkg = model.Package(
        name="foo",
        output_paths=[],
        nixpkgs_metadata=model.NixpkgsMetadata(
            pname=None, version="1.0", broken=False, license="MIT"
        ),
        build_inputs=[],
    )
    with pytest.raises(IngestionError):
        core_to_graph_model(pkg)


def test_unit_traverse_visits_all_nodes():
    """Check traverse visits all nodes in the input graph"""
    from explorer.api.ingest import traverse

    visited_root_pkgs: list[str] = []
    visited_pkgs: list[str] = []

    def visit_root_node(p: graph.Package):
        visited_root_pkgs.append(p.outputPath)

    def visit_layer(p: graph.Package, up: graph.Package, _e: graph.Edge):
        visited_pkgs.append(p.outputPath)
        visited_pkgs.append(up.outputPath)

    traverse(dummy_nix_graph, visit_root_node, visit_layer)

    expected_root_pkgs = set(map(lambda x: x.path, dummy_pkg_a.output_paths))
    expected_pkgs = set(
        map(
            lambda x: x.path,
            dummy_pkg_a.output_paths
            + dummy_pkg_b.output_paths
            + dummy_pkg_c.output_paths
            + dummy_pkg_d.output_paths,
        )
    )
    assert set(visited_root_pkgs) == expected_root_pkgs
    assert set(visited_pkgs) == expected_pkgs


def test_unit_ingest_nix_graph(graph_connection: DriverRemoteConnection):
    """Check that all expected nodes and edges are created when ingesting a nix graph"""
    from explorer.api.ingest import ingest_nix_graph

    g = traversal().with_remote(graph_connection)
    ingest_nix_graph(dummy_nix_graph, g)

    n_nodes = g.V().count().to_list()
    n_edges = g.E().count().to_list()

    # TODO: It might be worth further tightening the assertions here in the future
    # since checking the count is only a rough approximation of correctness.
    assert n_nodes == [4]
    assert n_edges == [3]


# TODO: Might want to add a test for cyclical dependencies if possible
#   (perhaps using networkx?)

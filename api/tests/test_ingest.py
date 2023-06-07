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
    attribute_path="D",
    output_path="/nix/store/D",
    derivation_path="/nix/store/D.drv",
    output_paths=[model.OutputPath(name="out", path="/D")],
    parsed_name=model.ParsedName(name="D", version="1.0"),
    nixpkgs_metadata=model.NixpkgsMetadata(
        pname="D", version="1.0", broken=False, license="MIT"
    ),
    build_inputs=[],
)


dummy_pkg_c = model.Package(
    name="C",
    attribute_path="C",
    output_path="/nix/store/C",
    derivation_path="/nix/store/C.drv",
    output_paths=[model.OutputPath(name="out", path="/C")],
    parsed_name=model.ParsedName(name="C", version="1.0"),
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
    attribute_path="B",
    output_path="/nix/store/B",
    derivation_path="/nix/store/B.drv",
    output_paths=[model.OutputPath(name="out", path="/B")],
    parsed_name=model.ParsedName(name="B", version="1.0"),
    nixpkgs_metadata=model.NixpkgsMetadata(
        pname="B", version="1.0", broken=False, license="MIT"
    ),
    build_inputs=[
        model.BuildInput(
            attribute_path="B.buildInputs.0",
            build_input_type=model.BuildInputType.BUILD_INPUT,
            output_path=dummy_pkg_c.output_paths[0].path,
        )
    ],
)

dummy_pkg_a = model.Package(
    name="A",
    attribute_path="A",
    output_path="/nix/store/A",
    derivation_path="/nix/store/A.drv",
    output_paths=[model.OutputPath(name="out", path="/A")],
    parsed_name=model.ParsedName(name="A", version="1.0"),
    nixpkgs_metadata=model.NixpkgsMetadata(
        pname="A", version="1.0", broken=False, license="MIT"
    ),
    build_inputs=[
        model.BuildInput(
            attribute_path="A.buildInputs.0",
            build_input_type=model.BuildInputType.BUILD_INPUT,
            output_path=dummy_pkg_b.output_paths[0].path,
        ),
        model.BuildInput(
            attribute_path="A.buildInputs.1",
            build_input_type=model.BuildInputType.BUILD_INPUT,
            output_path=dummy_pkg_d.output_paths[0].path,
        ),
    ],
)

dummy_nix_graph = model.NixGraph(
    packages=[dummy_pkg_a, dummy_pkg_b, dummy_pkg_c, dummy_pkg_d]
)


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
        attribute_path="foo",
        derivation_path="/nix/store/foo.drv",
        output_path="/nix/store/foo",
        parsed_name=model.ParsedName(name="foo", version="1.0"),
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
        attribute_path="foo",
        derivation_path="/nix/store/foo.drv",
        output_path="/nix/store/foo",
        output_paths=[],
        parsed_name=model.ParsedName(name="foo", version="1.0"),
        nixpkgs_metadata=model.NixpkgsMetadata(
            pname="foo", version="1.0", broken=False, license="MIT"
        ),
        build_inputs=[],
    )
    assert core_to_graph_model(pkg) == []


def test_unit_core_to_graph_model_no_parsed_name_raises():
    """Check that attempting to parse a package without nixpkgs_metadata fails"""
    from explorer.api.ingest import IngestionError, core_to_graph_model

    pkg = model.Package(
        name="foo",
        attribute_path="foo",
        derivation_path="/nix/store/foo.drv",
        output_path="/nix/store/foo",
        output_paths=[],
        parsed_name=None,
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
        attribute_path="foo",
        derivation_path="/nix/store/foo.drv",
        output_path="/nix/store/foo",
        output_paths=[],
        parsed_name=model.ParsedName(name=None, version="1.0"),
        nixpkgs_metadata=model.NixpkgsMetadata(
            pname=None, version="1.0", broken=False, license="MIT"
        ),
        build_inputs=[],
    )
    with pytest.raises(IngestionError):
        core_to_graph_model(pkg)


def test_unit_ingest_nix_graph(graph_connection: DriverRemoteConnection):
    """Check that all expected nodes and edges are created when ingesting a nix graph"""
    from explorer.api.ingest import ingest_nix_graph

    def g_factory():
        return traversal().with_remote(graph_connection)

    ingest_nix_graph(dummy_nix_graph, g_factory)

    n_nodes = g_factory().V().count().to_list()
    n_edges = g_factory().E().count().to_list()

    # TODO: It might be worth further tightening the assertions here in the future
    # since checking the count is only a rough approximation of correctness.
    assert n_nodes == [4]
    assert n_edges == [3]


# TODO: Might want to add a test for cyclical dependencies if possible
#   (perhaps using networkx?)

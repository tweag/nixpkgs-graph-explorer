from typing import Any, Callable, Mapping, cast

import pytest

from explorer.api import graph
from explorer.core import model


def test_unit_core_to_graph_model():
    from explorer.api.ingest import core_to_graph_model

    pkg = model.Package(
        name="foo",
        output_paths={
            model.OutputPathName.DEV: "/foo/dev",
            model.OutputPathName.LIB: "/foo/lib",
        },
        nixpkgs_metadata=model.NixpkgsMetadata(
            pname="foo", version="1.0", broken=False, license="MIT"
        ),
        build_inputs=[],
    )
    converted_pkgs = core_to_graph_model(pkg)

    expected_pkg_dev = graph.Package(pname="foo", outputPath="/foo/dev")
    expected_pkg_lib = graph.Package(pname="foo", outputPath="/foo/dev")
    assert expected_pkg_dev in converted_pkgs
    assert expected_pkg_lib in converted_pkgs


def test_unit_core_to_graph_model_no_output_path():
    from explorer.api.ingest import core_to_graph_model

    pkg = model.Package(
        name="foo",
        output_paths={},
        nixpkgs_metadata=model.NixpkgsMetadata(
            pname="foo", version="1.0", broken=False, license="MIT"
        ),
        build_inputs=[],
    )
    assert core_to_graph_model(pkg) == []


def test_unit_core_to_graph_model_no_metadata_raises():
    from explorer.api.ingest import core_to_graph_model, IngestionError

    pkg = model.Package(
        name="foo",
        output_paths={},
        nixpkgs_metadata=None,
        build_inputs=[],
    )
    with pytest.raises(IngestionError):
        core_to_graph_model(pkg)


def test_unit_core_to_graph_model_no_pname_raises():
    from explorer.api.ingest import core_to_graph_model, IngestionError

    pkg = model.Package(
        name="foo",
        output_paths={},
        nixpkgs_metadata=model.NixpkgsMetadata(
            pname=None, version="1.0", broken=False, license="MIT"
        ),
        build_inputs=[],
    )
    with pytest.raises(IngestionError):
        core_to_graph_model(pkg)


def mk_visitor_fn(
    accumulator: dict[str, int]
) -> Callable[[graph.Package, graph.Package | None], None]:
    def visitor_fn(
        current_package: graph.Package, upstream_package: graph.Package | None
    ):
        if current_package.outputPath not in accumulator:
            accumulator[current_package.outputPath] = 1
        else:
            accumulator[current_package.outputPath] += 1
        if upstream_package is None:
            return
        if upstream_package.outputPath not in accumulator:
            accumulator[upstream_package.outputPath] = 1
        else:
            accumulator[upstream_package.outputPath] += 1

    return visitor_fn


def test_unit_traverse_visits_all_nodes():
    from explorer.core.model import (
        NixGraph,
        Package,
        OutputPathName,
        NixpkgsMetadata,
        BuildInput,
        BuildInputType,
    )
    from explorer.api.ingest import traverse

    # Construct a graph for testing
    graph = NixGraph(
        packages=[
            Package(
                name="A",
                output_paths={OutputPathName.OUT: "/A"},
                nixpkgs_metadata=NixpkgsMetadata(
                    pname="A", version="1.0", broken=False, license="MIT"
                ),
                build_inputs=[
                    BuildInput(
                        build_input_type=BuildInputType.BUILD_INPUT,
                        package=Package(
                            name="B",
                            output_paths={OutputPathName.OUT: "/B"},
                            nixpkgs_metadata=NixpkgsMetadata(
                                pname="B", version="1.0", broken=False, license="MIT"
                            ),
                            build_inputs=[
                                BuildInput(
                                    build_input_type=BuildInputType.BUILD_INPUT,
                                    package=Package(
                                        name="C",
                                        output_paths={OutputPathName.OUT: "/C"},
                                        nixpkgs_metadata=NixpkgsMetadata(
                                            pname="C",
                                            version="1.0",
                                            broken=False,
                                            license="MIT",
                                        ),
                                        build_inputs=[],
                                    ),
                                )
                            ],
                        ),
                    ),
                    BuildInput(
                        build_input_type=BuildInputType.BUILD_INPUT,
                        package=Package(
                            name="D",
                            output_paths={OutputPathName.OUT: "/D"},
                            nixpkgs_metadata=NixpkgsMetadata(
                                pname="D", version="1.0", broken=False, license="MIT"
                            ),
                            build_inputs=[],
                        ),
                    ),
                ],
            )
        ]
    )

    accumulator = {}
    do_fn = mk_visitor_fn(accumulator)
    traverse(graph, do_fn)

    expected = {
        "/A": 3,
        "/B": 2,
        "/C": 1,
        "/D": 1,
    }
    assert accumulator == expected


# TODO: Might want to add a test for cyclical dependencies if that is even possible?

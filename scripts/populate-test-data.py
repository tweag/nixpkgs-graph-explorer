"""
Helper script for populating test database with some mock data. It will do the following:

1. Delete all existing vertices and edges from the Gremlin server running at localhost:8182
2. Insert some dummy vertices and edges for development purposes.

Use with caution!
"""
from contextlib import closing
from typing import List
from flask import Flask, request, jsonify
from gremlin_python.process.anonymous_traversal import traversal
from gremlin_python.driver.driver_remote_connection import DriverRemoteConnection
from gremlin_python.driver.client import Client
from dataclasses import dataclass
from gremlin_python.process.graph_traversal import __, GraphTraversalSource


@dataclass
class Package:
    name: str
    output_path: str = "foobar"


@dataclass
class DependencyRelation:
    upstream: Package
    downstream: Package


def unique_insert(g: GraphTraversalSource, pkg: Package):
    return (
        g.V()
        .has("package", "outputPath", pkg.output_path)
        .fold()
        .coalesce(
            __.unfold(),
            __.addV("package")
            .property("outputPath", pkg.output_path)
            .property("path", pkg.output_path)
            .property("pname", pkg.name)
            .property("version", "1.0.0"),
        )
        .iterate()
    )


def draw_dependency_relation(g: GraphTraversalSource, dr: DependencyRelation):
    g.V().has("package", "outputPath", dr.upstream.output_path).addE(
        "dependsOn"
    ).inV().has("package", "outputPath", dr.downstream.output_path).iterate()


def main():
    package_a = Package("package-a", "/output/path/a")
    package_b = Package("package-b", "/output/path/b")
    package_c = Package("package-c", "/output/path/c")
    package_d = Package("package-d", "/output/path/d")
    package_e = Package("package-e", "/output/path/e")
    package_f = Package("package-f", "/output/path/f")

    dep_relations: List[DependencyRelation] = [
        DependencyRelation(upstream=package_a, downstream=package_b),
        DependencyRelation(upstream=package_b, downstream=package_c),
        DependencyRelation(upstream=package_b, downstream=package_d),
        DependencyRelation(upstream=package_e, downstream=package_f),
    ]

    with closing(DriverRemoteConnection("ws://localhost:8182/gremlin", "g")) as remote:
        g = traversal().withRemote(remote)

        print("Deleting existing graph elements...")
        g.V().drop().iterate()
        g.E().drop().iterate()
        print("Done.")

        print("Inserting test data...")
        for dr in dep_relations:
            unique_insert(g, dr.downstream)
            unique_insert(g, dr.upstream)
            draw_dependency_relation(g, dr)
        print("Done.")

        print("Number of vertices:")
        print(g.V().count().toList())
        print("Number of edges:")
        print(g.E().count().toList())


if __name__ == "__main__":
    main()

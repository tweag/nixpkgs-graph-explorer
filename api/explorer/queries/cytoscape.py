"""Cytoscape.js data model

This module implements a limited subset of the Cytoscape.js data model in Python.
This is useful for constructing cytoscape-compatible data, for example a JSON
object which can be read by Cytoscape.js.

Note that this data model aims to be compatible with Cytoscape.js ^3.19

The basis for this module was programmatically generated using datamodel-code-generator
(version 0.17.1) from the JSON Schema available at:
    https://github.com/AZaitzeff/cytoscape_js_schema
    (rev: 28aa3e15afb9870a0303dae87fe15c1a58361647)

"""

from __future__ import annotations

from enum import Enum
from typing import List, Optional, Union

from pydantic import BaseModel, Field


class Group(Enum):
    nodes = "nodes"
    edges = "edges"


class EdgeData(BaseModel):
    id: Optional[str] = Field(
        default=None,
        description=(
            "optional (string) id for each element, assigned automatically on undefined"
        ),
    )
    source: str = Field(
        description="the source node id (edge comes from this node)",
    )
    target: str = Field(description="the target node id (edge goes to this node)")
    # Note: cytoscape.js accepts an arbitrary number of other properties as well,
    # but we have not implemented them in the schema here


class NodeData(BaseModel):
    id: Optional[str] = Field(
        default=None,
        description=(
            "optional (string) id for each element, assigned automatically on undefined"
        ),
    )
    parent: Optional[str] = Field(
        default=None,
        description="indicates the compound node parent id; not defined => no parent",
    )
    # Note: cytoscape.js accepts an arbitrary number of other properties as well,
    # but we have not implemented them in the schema here


class Position(BaseModel):
    x: Optional[float] = None
    y: Optional[float] = None


class Element(BaseModel):
    data: Union[NodeData, EdgeData] = Field(None, description="element data")
    group: Optional[Group] = Field(
        default=None,
        description=(
            "nodes' for a node, 'edges' for an edge. NB the group field can be"
            " automatically inferred for you but specifying it gives you nice debug"
            " messages if you mis-init elements"
        ),
    )
    position: Optional[Position] = Field(
        default=None, description="the model position of the node"
    )


class NodeDefinition(Element):
    data: NodeData


class EdgeDefinition(Element):
    data: EdgeData


class ElementsDefinition(BaseModel):
    nodes: list[NodeDefinition]
    edges: list[EdgeDefinition]


class CytoscapeJs(BaseModel):
    elements: Optional[ElementsDefinition | List[Element]] = Field(
        default=None,
        description="The graph's elements",
    )

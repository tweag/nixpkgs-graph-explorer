from dataclasses import asdict, dataclass
from typing import Any, ClassVar, Dict, Mapping, Protocol, Tuple, Type
from typing_extensions import Self
from abc import ABC, abstractmethod
from gremlin_python.process.graph_traversal import (
    GraphTraversalSource,
    GraphTraversal,
    __,
)
from gremlin_python.process.traversal import T
from typing import NewType
from marshmallow import Schema

import marshmallow_dataclass


@dataclass
class GraphElement(ABC):
    @property
    @abstractmethod
    def label(self) -> str:
        pass

    @classmethod
    def schema(cls, *args, **kwargs) -> Schema:
        return marshmallow_dataclass.class_schema(cls)(*args, **kwargs)

    @classmethod
    def from_element_map(cls, element_map: Mapping[Any, Any], *args, **kwargs) -> Self:
        # Filter out Gremlin internal ID and label fields prior to attempting deserialization
        properties = {
            # Note: Disabled type-checking here since `T` does not play nicely with it
            k: v
            for k, v in element_map.items()
            if k != T.id and k != T.label  # type: ignore
        }
        deserialized_value = cls.schema(*args, **kwargs).load(properties)
        if not isinstance(deserialized_value, cls):
            raise Exception(
                "Deserialization from this class's schema did not return an instance of this class, "
                "but this is required by from_properties(). "
                "This should never happen unless you have manually override the definition of schema()."
            )
        return deserialized_value


ElementId = NewType("ElementId", str)


class UniqueGraphElement(GraphElement):
    @abstractmethod
    def getId(self) -> Tuple[ElementId, str]:
        pass


@dataclass
class Package(UniqueGraphElement):
    pname: str
    output_path: str

    @property
    def label(self) -> str:
        return "package"

    def getId(self) -> Tuple[ElementId, str]:
        return (ElementId(self.output_path), "output_path")


def _traversal_insert_vertex(e: GraphElement, g: GraphTraversal) -> GraphTraversal:
    # Extract dataclass properties
    properties = asdict(e)
    traversal = g.addV(e.label)
    for property_name, property_value in properties.items():
        # Note: if `property_value` cannot be converted to a valid Gremlin type by gremlin_python we may
        # end up with runtime errors here. Note really sure how to restrict this though...
        traversal = traversal.property(property_name, property_value)
    return traversal


def insert_vertex(e: GraphElement, g: GraphTraversalSource) -> None:
    _traversal_insert_vertex(e, g.get_graph_traversal()).iterate()


def insert_unique_vertex(e: UniqueGraphElement, g: GraphTraversalSource) -> None:
    element_id, element_id_field = e.getId()
    g.V().has(e.label, element_id_field, element_id).fold().coalesce(
        __.unfold(), _traversal_insert_vertex(e, __.start())
    ).iterate()

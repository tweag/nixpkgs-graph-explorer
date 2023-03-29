from abc import ABC, abstractmethod
from typing import Any, Mapping, NewType

from gremlin_python.process.graph_traversal import (
    GraphTraversal,
    GraphTraversalSource,
    __,
)
from gremlin_python.process.traversal import T
from pydantic import BaseModel
from typing_extensions import Self


class GraphElement(ABC, BaseModel):
    """Base class for any element in the nixpkgs-graph-explorer graph"""

    @classmethod
    @abstractmethod
    def label(cls) -> str:
        """Getter for the element's label

        Returns:
            str: the label
        """
        pass

    @classmethod
    def from_element_map(cls, element_map: Mapping[Any, Any]) -> Self:
        """Constructs a GraphElement from its properties

        Args:
            element_map (Mapping[Any, Any]): A mapping of the elements properties,
                for example as returned by a Gremlin elementMap() step.

        Raises:
            Exception: If the cls's parse_obj method does not return a cls instance
                as expected.

        Returns:
            Self: the element
        """
        # Filter out Gremlin internal ID and label fields prior to
        # attempting deserialization
        properties = {
            # Note: Disabled type-checking here since `T` does not play nicely with it
            k: v
            for k, v in element_map.items()
            if k != T.id and k != T.label  # type: ignore
        }
        deserialized_value: Self = cls.parse_obj(properties)
        if not isinstance(deserialized_value, cls):
            raise Exception(
                "Deserialization from this class's schema did not return "
                "an instance of this class, "
                "but this is required by from_properties(). "
                "This should never happen unless you have manually override "
                "the definition of schema()."
            )
        return deserialized_value


ElementId = NewType("ElementId", str)


class UniqueGraphElement(GraphElement):
    @classmethod
    @abstractmethod
    def id_property_name(cls) -> str:
        """Getter for the name of the property used as the element's unique identifier

        Returns:
            str: the property name
        """
        pass

    @abstractmethod
    def get_id(self) -> ElementId:
        """Getter for the property used as the element's unique identifier

        Returns:
            ElementId: the unique identifier
        """
        pass


class Package(UniqueGraphElement):
    """A package from nixpkgs"""

    pname: str
    outputPath: str

    @classmethod
    def label(cls) -> str:
        return "package"

    @classmethod
    def id_property_name(cls) -> str:
        return "outputPath"

    def get_id(self) -> str:
        return ElementId(self.outputPath)


def _traversal_insert_vertex(e: GraphElement, g: GraphTraversal) -> GraphTraversal:
    # Extract dataclass properties
    properties = e.dict()
    traversal = g.add_v(e.label())
    for property_name, property_value in properties.items():
        # Note: if `property_value` cannot be converted to a valid Gremlin type by
        # gremlin_python we may end up with runtime errors here. Note really sure how
        # to restrict this though...
        traversal = traversal.property(property_name, property_value)
    return traversal


def insert_vertex(e: GraphElement, g: GraphTraversalSource) -> None:
    """Inserts a vertex element using graph traversal source

    Args:
        e (GraphElement): the vertex element to insert
        g (GraphTraversalSource): the graph traversal source to use for the insertion
    """
    _traversal_insert_vertex(e, g.get_graph_traversal()).iterate()


def insert_unique_vertex(e: UniqueGraphElement, g: GraphTraversalSource) -> None:
    """
    Inserts a uniquely identifiable vertex if a vertex corresponding to it does not
    already exist in the graph. If an existing vertex is found, returns without
    modifying the graph.

    Args:
        e (UniqueGraphElement): the vertex element to insert
        g (GraphTraversalSource): the graph traversal source to use for the insertion
    """
    g.V().has(e.label(), e.id_property_name(), e.get_id()).fold().coalesce(
        __.unfold(), _traversal_insert_vertex(e, __.start())
    ).iterate()

from dataclasses import dataclass
from enum import Enum, auto

from explorer.graph import UniqueGraphElement


class CursorDirection(Enum):
    PREVIOUS = auto()
    NEXT = auto()


@dataclass
class Cursor:
    row_id: str
    direction: CursorDirection = CursorDirection.NEXT

    @classmethod
    def from_unique_element(
        cls,
        element: UniqueGraphElement,
        direction: CursorDirection = CursorDirection.NEXT,
    ):
        return cls(row_id=element.get_id(), direction=direction)

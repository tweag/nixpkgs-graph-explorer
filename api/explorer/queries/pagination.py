from enum import Enum, auto

from pydantic import BaseModel

from explorer.graph import UniqueGraphElement


class CursorDirection(Enum):
    PREVIOUS = auto()
    NEXT = auto()


class Cursor(BaseModel):
    row_id: str
    direction: CursorDirection = CursorDirection.NEXT

    @classmethod
    def from_unique_element(
        cls,
        element: UniqueGraphElement,
        direction: CursorDirection = CursorDirection.NEXT,
    ):
        """Constructs a Cursor from a UniqueGraphElement using its id

        Args:
            element (UniqueGraphElement): The unique element
            direction (CursorDirection, optional): The direction of the cursor.
                Defaults to CursorDirection.NEXT.

        Returns:
            Cursor: The cursor
        """
        return cls(row_id=element.get_id(), direction=direction)

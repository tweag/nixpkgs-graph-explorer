from dataclasses import asdict, dataclass
from enum import Enum, auto


class CursorDirection(Enum):
    PREVIOUS = auto()
    NEXT = auto()


@dataclass
class Cursor:
    row_id: str
    direction: CursorDirection = CursorDirection.NEXT


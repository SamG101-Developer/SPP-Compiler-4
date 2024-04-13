from __future__ import annotations

from enum import Enum
from typing import List, Tuple


class SemanticErrorStringFormatType:
    NORMAL = 0
    MINIMAL = 1
    NO_FORMAT = 2


class SemanticError(Exception):
    additional_info: List[Tuple[int, str, SemanticErrorStringFormatType]]
    next_exceptions: List[SemanticError]

    def __init__(self, message: str) -> None:
        super().__init__(message)
        self.additional_info = []
        self.next_exceptions = []

    def add_traceback(self, pos: int, message: str, format_: SemanticErrorStringFormatType = SemanticErrorStringFormatType.NORMAL) -> SemanticError:
        self.additional_info.append((pos, message, format_))
        return self

    def __add__(self, other: SemanticError) -> SemanticError:
        self.next_exceptions.append(other)
        return self

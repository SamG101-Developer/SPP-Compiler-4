from __future__ import annotations

from typing import List, Tuple


class SemanticError(Exception):
    additional_info: List[Tuple[int, str, bool]]
    next_exceptions: List[SemanticError]

    def __init__(self, message: str) -> None:
        super().__init__(message)
        self.additional_info = []
        self.next_exceptions = []

    def add_traceback(self, pos: int, message: str) -> SemanticError:
        self.additional_info.append((pos, message, False))
        return self

    def add_traceback_minimal(self, pos: int, message: str) -> SemanticError:
        self.additional_info.append((pos, message, True))
        return self

    def __add__(self, other: SemanticError) -> SemanticError:
        self.next_exceptions.append(other)
        return self

from __future__ import annotations

from typing import List, Tuple


class SemanticError(Exception):
    additional_info: List[Tuple[int, str]]
    footer: str

    def __init__(self, message: str) -> None:
        super().__init__(message)
        self.additional_info = []
        self.footer = ""

    def add_traceback(self, pos: int, message: str) -> SemanticError:
        self.additional_info.append((pos, message))
        return self

    def add_footer(self, footer: str) -> SemanticError:
        assert not self.footer
        self.footer = footer
        return self

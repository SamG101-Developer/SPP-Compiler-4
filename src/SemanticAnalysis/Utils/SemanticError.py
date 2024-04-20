from __future__ import annotations

from colorama import Fore, Style
from enum import Enum
from typing import List, Tuple
import inflection


class SemanticErrorStringFormatType(Enum):
    NORMAL = 0
    MINIMAL = 1
    NO_FORMAT = 2


class SemanticErrorType(Enum):
    _NONE = -1
    TYPE_ERROR = 0
    VALUE_ERROR = 1
    LITERAL_ERROR = 2
    NAME_ERROR = 3
    ORDER_ERROR = 4
    MEMORY_ERROR = 5
    ASYNC_ERROR = 6


class SemanticError(Exception):
    additional_info: List[Tuple[int, str, str, SemanticErrorStringFormatType]]

    def __init__(self) -> None:
        super().__init__("")
        self.additional_info = []

    def add_error(self, pos: int, error_type: SemanticErrorType, message: str, tag_message: str, tip: str, format_: SemanticErrorStringFormatType = SemanticErrorStringFormatType.NORMAL) -> SemanticError:
        error_type = inflection.titleize(str(error_type).rsplit(".", 1)[-1].replace("_", " "))
        message = f"{Style.BRIGHT}{error_type}: {Style.NORMAL}{message}\n{Fore.LIGHTCYAN_EX}{Style.BRIGHT}Tip: {Style.NORMAL}{tip}"
        self.additional_info.append((pos, message, tag_message, format_))
        return self

    def add_info(self, pos: int, tag_message: str) -> SemanticError:
        self.add_error(pos, SemanticErrorType._NONE, "", tag_message, "", SemanticErrorStringFormatType.MINIMAL)
        return self

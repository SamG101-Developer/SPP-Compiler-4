from abc import ABC, abstractmethod
from typing import Iterator

from src.SemanticAnalysis.Symbols.Scopes import Scope


class SemanticAnalysis(ABC):
    @abstractmethod
    def do_semantic_analysis(self, s: Iterator[Scope]) -> None:
        pass

from abc import ABC, abstractmethod
from typing import Iterator

from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler


class SemanticAnalysis(ABC):
    @abstractmethod
    def do_semantic_analysis(self, s: Iterator[ScopeHandler]) -> None:
        pass

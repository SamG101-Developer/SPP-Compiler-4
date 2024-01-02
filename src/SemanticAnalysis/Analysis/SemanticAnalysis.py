from abc import ABC, abstractmethod

from src.SemanticAnalysis.Symbols.Scopes import ScopeIterator


class SemanticAnalysis(ABC):
    @abstractmethod
    def do_semantic_analysis(self, s: ScopeIterator) -> None:
        pass

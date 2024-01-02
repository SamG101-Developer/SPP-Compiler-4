from abc import ABC, abstractmethod

from src.SemanticAnalysis.Symbols.Scopes import ScopeIterator, ScopeHandler


class SemanticAnalysis(ABC):
    @abstractmethod
    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        pass

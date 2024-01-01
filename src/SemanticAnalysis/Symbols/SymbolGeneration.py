from abc import ABC, abstractmethod

from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler


class SymbolGenerator(ABC):
    @abstractmethod
    def generate(self, s: ScopeHandler) -> None:
        ...

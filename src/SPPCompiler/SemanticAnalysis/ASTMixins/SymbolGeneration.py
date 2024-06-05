from abc import ABC, abstractmethod

from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


class SymbolGenerator(ABC):
    @abstractmethod
    def generate(self, s: ScopeHandler) -> None:
        ...

from __future__ import annotations
from abc import ABC, abstractmethod

from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler


class TypeInfer(ABC):
    @abstractmethod
    def infer_type(self, scope_handler: ScopeHandler) -> TypeAst:
        pass

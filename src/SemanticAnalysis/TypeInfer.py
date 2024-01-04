from __future__ import annotations
from abc import ABC, abstractmethod
from typing import Tuple, Type

from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler


class TypeInfer(ABC):
    @abstractmethod
    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        pass

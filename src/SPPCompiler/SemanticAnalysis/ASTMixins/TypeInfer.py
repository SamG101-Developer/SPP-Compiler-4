from __future__ import annotations
from abc import ABC, abstractmethod
from typing import Tuple, Type

from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


class TypeInfer(ABC):
    @abstractmethod
    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        pass

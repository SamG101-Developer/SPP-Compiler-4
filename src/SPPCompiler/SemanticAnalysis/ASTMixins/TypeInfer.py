from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Type

from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol


class TypeInfer(ABC):
    @abstractmethod
    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        pass


@dataclass(kw_only=True)
class InferredType:
    convention: Type[ConventionAst]
    type_symbol: TypeSymbol

    def __post_init__(self):
        if self.type_symbol is None:
            import inspect
            frame = inspect.stack()[2]
            print(frame.function, frame.filename, frame.lineno)

    def symbolic_eq(self, other: InferredType, scope_handler: ScopeHandler) -> bool:
        return self.convention == other.convention and self.type_symbol.type == other.type_symbol.type

    def __str__(self):
        return f"{self.convention}{self.type_symbol.name}"

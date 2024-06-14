from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Optional, Type

from SPPCompiler.SemanticAnalysis.Utils.Scopes import Scope, ScopeHandler


class TypeInfer(ABC):
    @abstractmethod
    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        pass


@dataclass(kw_only=True)
class InferredType:
    convention: Type["ConventionAst"]
    type: "TypeAst"

    def symbolic_eq(self, other: InferredType, scope: Scope, that_scope: Optional[Scope] = None) -> bool:
        return self.convention == other.convention and self.type.symbolic_eq(other.type, scope, that_scope)

    def __str__(self):
        return f"{self.convention}{self.type}"

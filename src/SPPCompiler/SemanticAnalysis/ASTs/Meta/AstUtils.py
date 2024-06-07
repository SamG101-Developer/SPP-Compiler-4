from __future__ import annotations
import copy

from SPPCompiler.LexicalAnalysis.Tokens import TokenType

from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorStringFormatType, SemanticErrorType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from SPPCompiler.Utils.Sequence import Seq


class AstUtils:
    @staticmethod
    def ensure_memory_integrity_of_expression(
            expression: ExpressionAst,
            scope_handler: ScopeHandler,
            **kwargs) -> None:

        ...


__all__ = ["AstUtils"]

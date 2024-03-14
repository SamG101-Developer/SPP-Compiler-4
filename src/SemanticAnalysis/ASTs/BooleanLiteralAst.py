from dataclasses import dataclass
from typing import Tuple, Type

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Types.CommonTypes import CommonTypes
from src.SemanticAnalysis.Types.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst


@dataclass
class BooleanLiteralAst(Ast, SemanticAnalysis, TypeInfer):
    """
    The BooleanLiteralAst node represents a boolean value, which can be either `true` or `false`.

    Attributes:
        - boolean: The boolean value.
    """

    boolean: bool

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the BooleanLiteralAst.
        s = ""
        s += f"{"true" if self.boolean else "false"}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # No semantic analysis is required for a boolean literal.
        assert self.boolean in [True, False]

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # A boolean is always of type `std.Bool`.
        return ConventionMovAst, CommonTypes.bool(self.pos)


__all__ = ["BooleanLiteralAst"]

from dataclasses import dataclass
from typing import Tuple, Type

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Types.TypeInfer import TypeInfer
from src.SemanticAnalysis.Types.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst


@dataclass
class ArrayLiteralEmptyAst(Ast, SemanticAnalysis, TypeInfer):
    """
    The ArrayLiteralEmptyAst class is the AST for an empty array literal. This means that the array has no elements, so
    the element type `T` cannot be inferred. This is gotten around by specifying the type inside the "[]" brackets.

    Attributes:
        - bracket_l_token: The left bracket token.
        - element_type: The elements' type.
        - bracket_r_token: The right bracket token.
    """

    bracket_l_token: TokenAst
    element_type: TypeAst
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ArrayLiteralEmptyAst.
        s = ""
        s += f"{self.bracket_l_token.print(printer)}"
        s += f"{self.element_type.print(printer)}"
        s += f"{self.bracket_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the element type.
        self.element_type.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # The array's type is `std.Arr[T]`, where `T` is the elements' given type.
        return ConventionMovAst, CommonTypes.arr(self.element_type, pos=self.pos)


__all__ = ["ArrayLiteralEmptyAst"]

from dataclasses import dataclass
from typing import Tuple, Type

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst
from SPPCompiler.SemanticAnalysis.ASTs.TypeAst import TypeAst


@dataclass
class ArrayLiteralEmptyAst(Ast, SemanticAnalyser, TypeInfer):
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
        ...

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # The array's type is `std.Arr[T]`, where `T` is the elements' given type.
        return ConventionMovAst, CommonTypes.arr(self.element_type, pos=self.pos)


__all__ = ["ArrayLiteralEmptyAst"]

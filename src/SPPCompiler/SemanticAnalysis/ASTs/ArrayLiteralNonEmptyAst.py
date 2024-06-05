from dataclasses import dataclass
from typing import List, Tuple, Type

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorStringFormatType, SemanticErrorType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ArrayLiteralNonEmptyAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The ArrayLiteralNonEmptyAst class is the AST for a non-empty array literal. This means that the array has at least
    one element, and the elements are used to determine the type that the generic parameter `T` maps to, in the `Arr[T]`
    type.

    Attributes:
        - bracket_l_token: The left bracket token.
        - elements: The elements of the array.
        - bracket_r_token: The right bracket token.
    """

    bracket_l_token: "TokenAst"
    elements: List["ExpressionAst"]
    bracket_r_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ArrayLiteralNonEmptyAst.
        s = ""
        s += f"{self.bracket_l_token.print(printer)}"
        s += Seq(self.elements).print(printer, ", ")
        s += f"{self.bracket_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # The array's type is `std.Arr[T]`, where `T` is the elements' inferred type.
        element_type = self.elements[0].infer_type(scope_handler, **kwargs)[1]
        return ConventionMovAst, CommonTypes.arr(element_type, pos=self.pos)


__all__ = ["ArrayLiteralNonEmptyAst"]

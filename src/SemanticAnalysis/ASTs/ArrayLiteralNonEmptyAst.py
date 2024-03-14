from dataclasses import dataclass
from typing import List, Tuple, Type

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Types.TypeInfer import TypeInfer
from src.SemanticAnalysis.Types.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

from src.Utils.Sequence import Seq


@dataclass
class ArrayLiteralNonEmptyAst(Ast, SemanticAnalysis, TypeInfer):
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
        # Analyze the elements of the array.
        Seq(self.elements).for_each(lambda i: i.do_semantic_analysis(scope_handler, **kwargs))

        # Check if all the elements have the same type (should only be 1 unique type).
        non_matching_types = Seq(self.elements).map(lambda i: i.infer_type(scope_handler, **kwargs)).unique_items()
        if non_matching_types.length > 1:
            exception = SemanticError(f"Array items must have the same type:")
            exception.add_traceback(non_matching_types[0][1].pos, f"Item '{non_matching_types[0][0]}{non_matching_types[0][1]}' found here.")
            exception.add_traceback(non_matching_types[1][1].pos, f"Item '{non_matching_types[1][0]}{non_matching_types[1][1]}' has a different type.")
            raise exception

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # The array's type is `std.Arr[T]`, where `T` is the elements' inferred type.
        element_type = self.elements[0].infer_type(scope_handler, **kwargs)[1]
        return ConventionMovAst, CommonTypes.arr(element_type, pos=self.pos)


__all__ = ["ArrayLiteralNonEmptyAst"]

from __future__ import annotations

from dataclasses import dataclass

from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst
from src.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst


@dataclass
class TypeArrayAst(Ast):
    """
    The TypeArrayAst node is a temporary node used to represent an array type from parsing. It is converted to a
    TypeArrayAst node of the type "std.Arr[T]".

    Attributes:
        - brack_l_token: The left bracket token.
        - items: The type being stored inside the array.
        - brack_r_token: The right bracket token.
    """

    brack_l_token: TokenAst
    internal_type: TypeAst
    brack_r_token: TokenAst

    def as_single_type(self) -> TypeSingleAst:
        # Convert the TypeTupleAst to a TypeSingleAst.
        return CommonTypes.arr(self.internal_type, self.brack_l_token.pos)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypeTupleAst.
        s = f"{self.brack_l_token.print(printer)}{self.internal_type.print(printer)}{self.brack_r_token.print(printer)}"
        return s


__all__ = ["TypeTupleAst"]

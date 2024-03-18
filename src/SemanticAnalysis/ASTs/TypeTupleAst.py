from __future__ import annotations

from dataclasses import dataclass
from typing import List
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst
from src.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst

from src.Utils.Sequence import Seq


@dataclass
class TypeTupleAst(Ast):
    """
    The TypeTupleAst node is a temporary node used to represent a tuple type from parsing. It is converted to a
    TypeSingleAst node of the type "std.Tup[..Ts]".

    Attributes:
        - paren_l_token: The left parenthesis token.
        - items: The types of the tuple.
        - paren_r_token: The right parenthesis token.
    """

    paren_l_token: TokenAst
    items: List[TypeAst]
    paren_r_token: TokenAst

    def as_single_type(self) -> TypeSingleAst:
        # Convert the TypeTupleAst to a TypeSingleAst.
        return CommonTypes.tuple(self.items)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypeTupleAst.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += Seq(self.items).print(printer, ", ")
        s += f"{self.paren_r_token.print(printer)}"
        return s


__all__ = ["TypeTupleAst"]

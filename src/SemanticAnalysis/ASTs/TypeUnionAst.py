from __future__ import annotations

from dataclasses import dataclass
from typing import List
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.TypeAst import TypeAst
from src.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst

from src.Utils.Sequence import Seq


@dataclass
class TypeUnionAst(Ast):
    """
    The TypeUnionAst node is a temporary node used to represent a union type from parsing. It is converted to a
    TypeSingleAst node of the type "std.Var[..Ts]" (variant type).

    Attributes:
        - items: The types in the union.
    """

    items: List[TypeAst]

    def as_single_type(self) -> TypeSingleAst:
        # Convert the TypeUnionAst to a TypeSingleAst.
        return CommonTypes.union(self.items)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypeUnionAst.
        s = ""
        s += f"{Seq(self.items).print(printer, " | ")}"
        return s


__all__ = ["TypeUnionAst"]

from __future__ import annotations

from dataclasses import dataclass
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst
from src.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst


@dataclass
class TypeOptionalAst(Ast):
    """
    The TypeOptionalAst node is a temporary node used to represent an optional type from parsing. It is converted to a
    TypeSingleAst node of the type "std.Opt[T]".

    Attributes:
        - type: The internal type.
        - question_mark_token: The question mark token.
    """

    question_mark_token: TokenAst
    type: TypeAst

    def as_single_type(self) -> TypeSingleAst:
        # Convert the TypeTupleAst to a TypeSingleAst.
        return CommonTypes.opt(self.type, self.type.pos)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypeTupleAst.
        s = f"{self.type.print(printer)}{self.question_mark_token.print(printer)}"
        return s


__all__ = ["TypeOptionalAst"]

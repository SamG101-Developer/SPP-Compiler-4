from dataclasses import dataclass
from typing import List

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.TypePartAst import TypePartAst

from src.Utils.Sequence import Seq


@dataclass
class TypedefStatementOldNamespaceAst(Ast):
    """
    The TypedefStatementOldNamespaceAst node is used to separate the namespace from the type being used, because either
    1, n or * items can be used from the namespace, so the separation is required.

    Attributes:
        - items: The items being used from the namespace.
    """

    items: List[TypePartAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypedefStatementOldNamespaceAst.
        s = ""
        s += f"{Seq(self.items).print(printer, ".")}"
        return s


__all__ = ["TypedefStatementOldNamespaceAst"]

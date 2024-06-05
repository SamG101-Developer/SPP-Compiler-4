from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class TypedefStatementAllItemsAst(Ast):
    """
    The TypedefStatementAllItemsAst allows for the use of all items in a namespace. For example, "use namespace.*"
    allows the use of all items in "namespace".

    Attributes:
        - all_token: The "*" token.
    """

    all_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypedefStatementAllItemsAst.
        s = ""
        s += f"{self.all_token.print(printer)}"
        return s


__all__ = ["TypedefStatementAllItemsAst"]

from dataclasses import dataclass

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class LocalVariableSkipArgumentAst(Ast):
    """
    The LocalVariableSkipArgumentAst node represents a placeholder variable that is skipping values. This is used inside
    tuple and object destructuring. For example, in the statement "let Point(x, ..) = point", ".." is the skip argument,
    skipping all other fields in the Point class.

    Attributes:
        - variadic_token: The ".." token.
    """

    variadic_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LocalVariableSkipArgumentAst.
        s = ""
        s += f"{self.variadic_token.print(printer)}"
        return s

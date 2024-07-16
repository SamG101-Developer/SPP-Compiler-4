from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class LocalVariableSkipArgumentsAst(Ast):
    """
    The LocalVariableSkipArgumentsAst node represents a placeholder variable that is skipping values. This is used
    inside tuple and object destructuring. For example, in the statement "let Point(x, ..) = point", ".." is the skip
    argument, skipping all other fields in the "Point" class.

    Attributes:
        variadic_token: The ".." token.
    """

    variadic_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LocalVariableSkipArgumentAst.
        s = ""
        s += f"{self.variadic_token.print(printer)}"
        return s


__all__ = ["LocalVariableSkipArgumentsAst"]

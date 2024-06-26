from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class PatternVariantSkipArgumentAst(Ast):
    """
    The PatternVariantSkipArgumentAst node represents the skipping of arguments in a tuple or object destructuring on a
    conditional branch. This is used to skip some arguments when not all parts are desired. For example,
    "case point then == Point(x, ..)" would destructure the "point" into "x" and skip the other fields like "y" and "z".

    Attributes:
        variadic_token: The variadic token.
    """

    variadic_token: "TokenAst"

    def convert_to_variable(self) -> "LocalVariableSkipArgumentAst":
        from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableSkipArgumentAst import LocalVariableSkipArgumentAst

        # Return the new LocalVariableSkipArgumentAst.
        bindings = LocalVariableSkipArgumentAst(
            pos=self.pos,
            variadic_token=self.variadic_token)

        return bindings

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternVariantSkipArgumentAst.
        s = ""
        s += f"{self.variadic_token.print(printer)}"
        return s


__all__ = ["PatternVariantSkipArgumentAst"]

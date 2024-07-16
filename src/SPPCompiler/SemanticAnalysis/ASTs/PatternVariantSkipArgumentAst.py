from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class PatternVariantSkipArgumentAst(Ast):
    """
    The PatternVariantSkipArgumentAst node represents the skipping of an argument in a tuple on a conditional branch.
    This is used to skip an argument when not all parts are desired. For example, "case tuple then == (a, _, b, 10)"
    would destructure the "point" into "x" and skip the other fields like "y" and "z".

    Attributes:
        underscore_token: The variadic token.
    """

    underscore_token: "TokenAst"

    def convert_to_variable(self) -> "LocalVariableSkipArgumentAst":
        from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableSkipArgumentAst import LocalVariableSkipArgumentAst

        # Return the new LocalVariableSkipArgumentAst.
        bindings = LocalVariableSkipArgumentAst(
            pos=self.pos,
            underscore_token=self.underscore_token)

        return bindings

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternVariantSkipArgumentAst.
        s = ""
        s += f"{self.underscore_token.print(printer)}"
        return s


__all__ = ["PatternVariantSkipArgumentAst"]

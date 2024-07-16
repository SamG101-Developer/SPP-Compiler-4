from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class LocalVariableSkipArgumentAst(Ast):
    """
    The LocalVariableSkipArgumentAst node represents a placeholder variable that is skipping a value. This is used
    inside tuple destructuring. For example, in the statement "let (a, _, b, _) = tuple", "_" is the skip argument,
    skipping other fields in the "tuple" object".

    Attributes:
        underscore_token: The "_" token.
    """

    underscore_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LocalVariableSkipArgumentAst.
        s = ""
        s += f"{self.underscore_token.print(printer)}"
        return s


__all__ = ["LocalVariableSkipArgumentAst"]

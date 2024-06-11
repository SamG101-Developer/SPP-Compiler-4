from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class LambdaCaptureItemNamedAst(Ast):
    """
    The LambdaCaptureItemNamedAst node is used to represent a named lambda capture item. A named capture has an
    identifier, convention and a value; the the identifier is assigned the value inside the lambda, like a fixed
    argument parameter.

    Attributes:
        - convention: The convention of the capture.
        - value: The value of the capture.
    """

    identifier: IdentifierAst
    assignment_token: TokenAst
    convention: ConventionAst
    value: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LambdaCaptureItemNamedAst.
        s = ""
        s += f"{self.identifier.print(printer)} "
        s += f"{self.assignment_token.print(printer)} "
        s += f"{self.convention.print(printer)}"
        s += f"{self.value.print(printer)}"
        return s


__all__ = ["LambdaCaptureItemNamedAst"]

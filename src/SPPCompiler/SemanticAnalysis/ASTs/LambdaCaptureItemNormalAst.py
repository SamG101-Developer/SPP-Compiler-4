from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst


@dataclass
class LambdaCaptureItemNormalAst(Ast):
    """
    The LambdaCaptureItemNormalAst node is used to represent a normal lambda capture item. A normal capture has a
    convention and a value; the value must be an identifier who can have the convention applied to it.

    Attributes:
        - convention: The convention of the capture.
        - value: The value of the capture.
    """

    convention: ConventionAst
    value: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LambdaCaptureItemNormalAst.
        s = ""
        s += f"{self.convention.print(printer)}"
        s += f"{self.value.print(printer)}"
        return s


__all__ = ["LambdaCaptureItemNormalAst"]

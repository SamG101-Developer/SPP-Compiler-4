from __future__ import annotations
from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class ConventionMovAst(Ast):
    """
    The ConventionMovAst node represents the convention for moving or copying an argument into a function parameter for
    a function call. It is also used to yield an owned value out of a function.
    """

    @staticmethod
    def dummy() -> ConventionMovAst:
        # Quick way to create a convention mov ast.
        return ConventionMovAst(pos=-1)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ConventionMovAst.
        return f""

    def __eq__(self, other):
        # Check both ASTs are the same type.
        return isinstance(other, ConventionMovAst)


__all__ = ["ConventionMovAst"]

from abc import ABCMeta
from dataclasses import dataclass

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *


class ConventionMovAstMeta(ABCMeta):
    def __repr__(self):
        return ""


@dataclass
class ConventionMovAst(Ast, metaclass=ConventionMovAstMeta):
    """
    The ConventionMovAst node represents the convention for moving or copying an argument into a function parameter for
    a function call. It is also used to yield an owned value out of a function.
    """

    def print(self, printer: AstPrinter) -> str:
        # Print the ConventionMovAst.
        return f""

    def __eq__(self, other):
        # Check both ASTs are the same type.
        return isinstance(other, ConventionMovAst)


__all__ = ["ConventionMovAst"]

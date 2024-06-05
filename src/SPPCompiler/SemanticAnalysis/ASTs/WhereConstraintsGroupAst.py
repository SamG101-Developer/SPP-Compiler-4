from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst
from SPPCompiler.SemanticAnalysis.ASTs.WhereConstraintsAst import WhereConstraintsAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class WhereConstraintsGroupAst(Ast):
    """
    The WhereConstraintsGroupAst node is used to group a number of WhereConstraintsAst nodes.

    Attributes:
        - brack_l_token: The left bracket token.
        - constraints: The constraints of the types.
        - brack_r_token: The right bracket token.
    """

    brack_l_token: TokenAst
    constraints: List[WhereConstraintsAst]
    brack_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the WhereConstraintsGroupAst.
        s = ""
        s += f"{self.brack_l_token.print(printer)}"
        s += Seq(self.constraints).print(printer, ", ")
        s += f"{self.brack_r_token.print(printer)}"
        return s

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same constraints.
        return isinstance(other, WhereConstraintsGroupAst) and self.constraints == other.constraints


__all__ = ["WhereConstraintsGroupAst"]

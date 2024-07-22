from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.LambdaCaptureItemAst import LambdaCaptureItemAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class LambdaCaptureBlockAst(Ast):
    """
    The LambdaCaptureBlockAst node represents a lambda capture block. It contains a list of normal and named capture
    items.

    Attributes:
        - bracket_l_token: The '{' token.
        - items: The items of the lambda capture block.
        - bracket_r_token: The '}' token.
    """

    bracket_l_token: TokenAst
    items: List[LambdaCaptureItemAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LambdaCaptureBlockAst.
        s = ""
        s += f"{self.bracket_l_token.print(printer)}"
        s += f"{Seq(self.items).print(printer, ", ")}"
        s += f"{self.bracket_r_token.print(printer)}"
        return s

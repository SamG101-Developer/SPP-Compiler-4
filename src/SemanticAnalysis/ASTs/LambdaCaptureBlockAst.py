from dataclasses import dataclass
from typing import List

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.LambdaCaptureItemAst import LambdaCaptureItemAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst

from src.Utils.Sequence import Seq


@dataclass
class LambdaCaptureBlockAst(Ast):
    """
    The LambdaCaptureBlockAst node represents a lambda capture block. It contains a list of normal and named capture
    items.

    Attributes:
        - with_keyword: The 'with' keyword token.
        - bracket_l_token: The '{' token.
        - items: The items of the lambda capture block.
        - bracket_r_token: The '}' token.
    """

    with_keyword: TokenAst
    bracket_l_token: TokenAst
    items: List[LambdaCaptureItemAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LambdaCaptureBlockAst.
        s = ""
        s += f"{self.with_keyword.print(printer)}"
        s += f"{self.bracket_l_token.print(printer)}"
        s += f"{Seq(self.items).print(printer, ", ")}"
        s += f"{self.bracket_r_token.print(printer)}"
        return s

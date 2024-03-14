from dataclasses import dataclass
from typing import List

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst

from src.Utils.Sequence import Seq


@dataclass
class LocalVariableTupleAst(Ast):
    """
    The LocalVariableTupleAst node represents a tuple of local variables. This is an advanced form of a local variable,
    and is seen mostly in the "let" statement. For example, in the statement "let (mut x, y) = (5, 6)", "(mut x, y)" is
    the tuple of local variables. Both "mut x" and "y" are separate single local variables.

    Attributes:
        - paren_l_token: The left parenthesis token.
        - items: The local variables in the tuple.
        - paren_r_token: The right parenthesis token.
    """

    paren_l_token: TokenAst
    items: List[LocalVariableSingleAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LocalVariableTupleAst.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.items).print(printer, ", ")}"
        s += f"{self.paren_r_token.print(printer)}"
        return s


__all__ = ["LocalVariableTupleAst"]

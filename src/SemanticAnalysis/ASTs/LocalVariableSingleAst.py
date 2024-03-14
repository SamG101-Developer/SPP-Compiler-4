from dataclasses import dataclass
from typing import Optional

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class LocalVariableSingleAst(Ast):
    """
    The LocalVariableSingleAst node represents a single local variable. This is the most basic form of a local variable,
    and is seen mostly in the "let" statement. For example, in the statement "let mut x = 5", "mut x" is the single
    local variable. Single variable can unpack tuples too, todo when?

    Attributes:
        - is_mutable: The token that represents the mutability of the variable.
        - unpack_token: The optional unpack token.
        - identifier: The identifier of the variable.
    """

    is_mutable: Optional[TokenAst]
    unpack_token: Optional[TokenAst]
    identifier: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LocalVariableSingleAst.
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.unpack_token.print(printer)}" if self.unpack_token else ""
        s += f"{self.identifier.print(printer)}"
        return s


__all__ = ["LocalVariableSingleAst"]

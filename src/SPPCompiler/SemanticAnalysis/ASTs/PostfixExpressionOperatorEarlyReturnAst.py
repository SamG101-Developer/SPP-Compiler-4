from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class PostfixExpressionOperatorEarlyReturnAst(Ast):
    """
    The PostfixExpressionOperatorEarlyReturnAst is an AST representing the early return postfix expression operator.

    Attributes:
        - return_token: The "?" token representing the early return operator.
    """

    return_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the early return operator.
        s = ""
        s += f"{self.return_token.print(printer)}"
        return s

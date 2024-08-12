from __future__ import annotations
from dataclasses import dataclass

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class ConventionRefAst(Ast):
    """
    The ConventionRefAst node represents the convention taking an immutable borrow of an argument into a function
    parameter for a function call. It is also used to yield an immutable borrow out of a coroutine.

    Attributes:
        ampersand_token: The ampersand token.
    """

    ampersand_token: TokenAst

    @staticmethod
    def dummy() -> ConventionRefAst:
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst

        # Quick way to create a convention ref ast.
        return ConventionRefAst(
            pos=-1,
            ampersand_token=TokenAst.dummy(TokenType.TkBorrow))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ConventionRefAst.
        s = ""
        s += f"{self.ampersand_token.print(printer)}"
        return s

    def __eq__(self, other):
        # Check both ASTs are the same type.
        return isinstance(other, ConventionRefAst)


__all__ = ["ConventionRefAst"]

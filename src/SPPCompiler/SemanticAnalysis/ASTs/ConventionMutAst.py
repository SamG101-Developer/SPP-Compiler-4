from __future__ import annotations
from dataclasses import dataclass

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class ConventionMutAst(Ast):
    """
    The ConventionMutAst node represents the convention taking a mutable borrow of an argument into a function parameter
    for a function call. It is also used to yield a mutable borrow out of a coroutine.

    Attributes:
        ampersand_token: The ampersand token.
        mut_token: The mut token.
    """

    ampersand_token: TokenAst
    mut_token: TokenAst

    @staticmethod
    def dummy() -> ConventionMutAst:
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst

        # Quick way to create a convention mut ast.
        return ConventionMutAst(
            pos=-1,
            ampersand_token=TokenAst.dummy(TokenType.TkBorrow),
            mut_token=TokenAst.dummy(TokenType.KwMut))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ConventionMutAst.
        s = ""
        s += f"{self.ampersand_token.print(printer)}"
        s += f"{self.mut_token.print(printer)} "
        return s

    def __eq__(self, other):
        # Check both ASTs are the same type.
        return isinstance(other, ConventionMutAst)


__all__ = ["ConventionMutAst"]

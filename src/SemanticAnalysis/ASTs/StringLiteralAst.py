from dataclasses import dataclass
from typing import Tuple, Type

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Types.TypeInfer import TypeInfer
from src.SemanticAnalysis.Types.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst


@dataclass
class StringLiteralAst(Ast, SemanticAnalysis, TypeInfer):
    """
    The StringLiteralAst node represents a string. It contains a TokenAst, which will be the LxDoubleQuoteStr lexeme
    token.

    Attributes:
        - value: The string token.
    """

    value: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the StringLiteralAst.
        s = ""
        s += f"{self.value.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # No semantic analysis is required for a string literal.
        assert self.value.token.token_type == TokenType.LxDoubleQuoteStr

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # The string literal's type is always `std.Str`.
        return ConventionMovAst, CommonTypes.str(pos=self.pos)

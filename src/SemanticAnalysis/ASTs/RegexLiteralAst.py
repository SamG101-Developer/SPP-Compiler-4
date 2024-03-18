from dataclasses import dataclass
from typing import Tuple, Type

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst


@dataclass
class RegexLiteralAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The RegexLiteralAst node represents a string. It contains a TokenAst, which will be the LxRegex lexeme token.

    Attributes:
        - value: The regex token.
    """

    value: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.value.print(printer)}"
        return s

    def do_semantic_analysis(self, **kwargs) -> None:
        # No semantic analysis is required for a string literal.
        assert self.value.token.token_type == TokenType.LxRegex

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # The string literal's type is always `std.Rgx`.
        return ConventionMovAst, CommonTypes.rgx(self.pos)

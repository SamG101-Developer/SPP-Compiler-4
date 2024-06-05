from dataclasses import dataclass
from typing import Tuple, Type

from SPPCompiler.LexicalAnalysis.Tokens import TokenType

from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst
from SPPCompiler.SemanticAnalysis.ASTs.TypeAst import TypeAst


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
        ...

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # The string literal's type is always `std.Rgx`.
        return ConventionMovAst, CommonTypes.rgx(self.pos)

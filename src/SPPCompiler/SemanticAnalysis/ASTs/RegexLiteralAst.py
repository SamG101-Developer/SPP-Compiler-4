from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class RegexLiteralAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The RegexLiteralAst node represents a string. It contains a TokenAst, which will be the LxRegex lexeme token.

    Attributes:
        value: The regex token.
    """

    value: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.value.print(printer)}"
        return s

    def do_semantic_analysis(self, **kwargs) -> None:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        assert self.value.token.token_type == TokenType.LxRegex

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
        return InferredType(
            convention=ConventionMovAst,
            type_symbol=scope_handler.current_scope.get_symbol(CommonTypes.rgx(self.pos)))

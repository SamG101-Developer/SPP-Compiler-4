from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *



@dataclass
class StringLiteralAst(Ast, SemanticAnalyser, TypeInfer):
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
        ...

    def infer_type(self, scope_handler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
        return InferredType(convention=ConventionMovAst, type=CommonTypes.str(pos=self.pos))

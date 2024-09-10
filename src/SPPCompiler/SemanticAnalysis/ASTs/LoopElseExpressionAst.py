from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class LoopElseExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The LoopElseExpressionAst node represents a while-else expression. This is used to execute a block of code if the
    condition for the WhileExpressionAst is already "false" at the first iteration.

    Attributes:
        else_keyword: The else keyword.
        body: The body of the else block.
    """

    else_keyword: "TokenAst"
    body: "InnerScopeAst[StatementAst]"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the WhileElseExpressionAst.
        s = ""
        s += f"{self.else_keyword.print(printer)}"
        s += f"{self.body.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the body.
        self.body.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        # The type is the body's type (final expression's type).
        return self.body.infer_type(scope_handler, **kwargs)


__all__ = ["LoopElseExpressionAst"]

from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class PatternVariantExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    """

    expression: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.expression}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        return self.expression.infer_type(scope_handler, **kwargs)


__all__ = ["PatternVariantExpressionAst"]

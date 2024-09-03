from dataclasses import dataclass, field

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class PatternVariantElseCaseAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The PatternVariantElseAst node represents an else branch. This is used to as a default branch when no other branches
    match. For example, "case point then == Point(x=0, y) {...} else {...}" could reach the else statement if the
    "point"s attribute "x" is not equal to 0.

    Attributes:
        else_token: The else token.
    """

    else_token: "TokenAst"
    case_expression: "CaseExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternVariantElseCaseAst. Todo
        s = ""
        s += f"{self.else_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the case expression.
        self.case_expression.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        # Infer the type of the else case.
        return self.case_expression.infer_type(scope_handler, **kwargs)


__all__ = ["PatternVariantElseCaseAst"]

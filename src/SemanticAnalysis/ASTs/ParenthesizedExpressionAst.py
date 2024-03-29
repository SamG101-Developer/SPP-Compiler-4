from dataclasses import dataclass
from typing import Tuple, Type

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class ParenthesizedExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The ParenthesizedExpressionAst node represents a parenthesized expression.

    Attributes:
        - paren_l_token: The left parenthesis token.
        - expression: The expression inside the parentheses.
        - paren_r_token: The right parenthesis token.
    """

    paren_l_token: "TokenAst"
    expression: "ExpressionAst"
    paren_r_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ParenthesizedExpressionAst.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{self.expression.print(printer)}"
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the expression inside the parentheses.
        self.expression.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, if_condition: "ExpressionAst" = None, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # Infer the type of the expression inside the parentheses.
        return self.expression.infer_type(scope_handler, **kwargs)


__all__ = ["ParenthesizedExpressionAst"]

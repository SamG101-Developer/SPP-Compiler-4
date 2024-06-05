from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class PatternVariantElseAst(Ast, SemanticAnalyser):
    """
    The PatternVariantElseAst node represents an else branch. This is used to as a default branch when no other branches
    match. For example, "case point then == Point(x=0, y) {...} else {...}" could reach the else statement if the
    "point"s attribute "x" is not equal to 0.
    
    Attributes:
        - else_token: The else token.
    """

    else_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternVariantElseAst.
        s = ""
        s += f"{self.else_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: "ExpressionAst" = None, **kwargs) -> None:
        ...


__all__ = ["PatternVariantElseAst"]

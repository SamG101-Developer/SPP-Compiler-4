from dataclasses import dataclass

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from src.SemanticAnalysis.ASTs.LocalVariableSkipArgumentAst import LocalVariableSkipArgumentAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class PatternVariantSkipArgumentAst(Ast, SemanticAnalyser):
    """
    The PatternVariantSkipArgumentAst node represents the skipping of arguments in a tuple or object destructuring on a
    conditional branch. This is used to skip some arguments when not all parts are desired. For example,
    "case point then == Point(x, ..)" would destructure the "point" into "x" and skip the other fields like "y" and "z".

    Attributes:
        - variadic_token: The variadic token.
    """

    variadic_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternVariantSkipArgumentAst.
        s = ""
        s += f"{self.variadic_token.print(printer)}"
        return s

    def convert_to_variable(self) -> LocalVariableSkipArgumentAst:
        # Return the new LocalVariableSkipArgumentAst.
        return LocalVariableSkipArgumentAst(
            pos=self.pos,
            variadic_token=self.variadic_token)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> None:
        # Analyse the conversion.
        conversion = self.convert_to_variable()
        conversion.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["PatternVariantSkipArgumentAst"]

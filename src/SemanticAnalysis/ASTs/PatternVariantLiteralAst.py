from dataclasses import dataclass
from typing import Tuple, Type

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from src.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from src.SemanticAnalysis.ASTs.LiteralAst import LiteralAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst


@dataclass
class PatternVariantLiteralAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The PatternVariantLiteralAst node represents a literal pattern on a conditional branch. This is used to match a
    value to a literal value. For example, "case point then == 1" would compare "point" to "1".

    Attributes:
        - literal: The literal being compared to.
    """

    literal: LiteralAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternVariantLiteralAst.
        s = ""
        s += f"{self.literal.print(printer)}"
        return s

    def convert_to_variable(self) -> LiteralAst:
        # Return the literal.
        return self.literal

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> None:
        # Analyse the literal.
        self.literal.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # The pattern's type is the literal's type.
        return self.literal.infer_type(scope_handler, **kwargs)

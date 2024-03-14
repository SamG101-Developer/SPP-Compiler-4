from dataclasses import dataclass
from typing import Optional, Tuple, Type

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Types.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from src.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst


@dataclass
class PatternVariantVariableAst(Ast, SemanticAnalysis, TypeInfer):
    """
    The PatternVariantVariableAst node represents a destructuring pattern on a conditional branch. This is used to
    match a value to a single variable. For example, "let Point(x, y) = point" would destructure the "point" into "x" and "y".

    Attributes:
        - class_type: The class type being destructured.
        - bracket_l_token: The left bracket token.
        - items: The items being destructured.
        - bracket_r_token: The right bracket token.
    """

    is_mutable: Optional[TokenAst]
    unpack_token: Optional[TokenAst]
    identifier: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternVariantVariableAst.
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.unpack_token.print(printer)}" if self.unpack_token else ""
        s += f"{self.identifier.print(printer)}"
        return s

    def convert_to_variable(self) -> LocalVariableSingleAst:
        # Return the new LocalVariableSingleAst.
        return LocalVariableSingleAst(
            pos=self.pos,
            is_mutable=self.is_mutable,
            unpack_token=self.unpack_token,
            identifier=self.identifier)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> None:
        conversion = self.convert_to_variable()
        # todo

    def infer_type(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        ...

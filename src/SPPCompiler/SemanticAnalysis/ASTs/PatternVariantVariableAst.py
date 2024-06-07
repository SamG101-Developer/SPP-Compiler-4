from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from SPPCompiler.SemanticAnalysis.ASTs.LocalVariableSingleAst import LocalVariableSingleAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class PatternVariantVariableAst(Ast, SemanticAnalyser, TypeInfer):
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

    def infer_type(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
        return InferredType(convention=ConventionMovAst, type=if_condition.infer_type(scope_handler, **kwargs).type)

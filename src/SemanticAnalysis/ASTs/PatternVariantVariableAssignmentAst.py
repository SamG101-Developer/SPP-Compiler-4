from dataclasses import dataclass
from typing import Tuple, Type

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Types.CommonTypes import CommonTypes
from src.SemanticAnalysis.Types.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.LocalVariableAssignmentAst import LocalVariableAssignmentAst


@dataclass
class PatternVariantVariableAssignmentAst(Ast, SemanticAnalysis, TypeInfer):
    """
    The PatternVariantLiteralAst node represents an assignment pattern on a conditional branch. This is used to match a
    value to a value whilst binding it to a variable. For example, "case point then == Point(x=0, y)" would bind
    destructure "point" into "x" and "y" as long as "x" is equal to 0.

    Attributes:
        - identifier: The identifier being assigned to.
        - assign_token: The assign token.
        - value: The value being assigned.
    """

    identifier: "IdentifierAst"
    assign_token: "TokenAst"
    value: "PatternVariantAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternVariantVariableAssignmentAst.
        s = ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.assign_token.print(printer)}"
        s += f"{self.value.print(printer)}"
        return s

    def convert_to_variable(self) -> "LocalVariableAssignmentAst":
        # Return the new LocalVariableAssignmentAst.
        return LocalVariableAssignmentAst(
            pos=self.pos,
            identifier=self.identifier,
            assign_token=self.assign_token,
            value=self.value.convert_to_variable())

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: "ExpressionAst" = None, **kwargs) -> None:
        # todo
        conversion = self.convert_to_variable()
        conversion.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, if_condition: "ExpressionAst" = None, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # The pattern's type is "Void", as all let statements return void.
        return ConventionMovAst, CommonTypes.void(self.pos)


__all__ = ["PatternVariantVariableAssignmentAst"]

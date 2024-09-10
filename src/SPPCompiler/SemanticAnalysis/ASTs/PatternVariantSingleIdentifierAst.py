from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class PatternVariantSingleIdentifierAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The PatternVariantVariableAst node represents a destructuring pattern on a conditional branch. This is used to
    match a value to a single variable. For example, "let Point(x, y) = point" would destructure the "point" into "x" and "y".

    Attributes:
        is_mutable: The mutable token.
        identifier: The identifier being destructured.
    """

    is_mutable: Optional["TokenAst"]
    identifier: "IdentifierAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternVariantVariableAst.
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.identifier.print(printer)}"
        return s

    def convert_to_variable(self) -> "LocalVariableSingleAst":
        from SPPCompiler.SemanticAnalysis.ASTs import LocalVariableSingleIdentifierAst

        # Return the new LocalVariableSingleAst.
        return LocalVariableSingleIdentifierAst(
            pos=self.pos,
            is_mutable=self.is_mutable,
            identifier=self.identifier)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        conversion = self.convert_to_variable()
        # todo

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
        return InferredType(convention=ConventionMovAst, type=kwargs["condition"].infer_type(scope_handler, **kwargs).type)


__all__ = ["PatternVariantSingleIdentifierAst"]

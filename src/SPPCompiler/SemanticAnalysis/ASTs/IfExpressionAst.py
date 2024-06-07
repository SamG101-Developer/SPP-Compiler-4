from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class IfExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The IfExpressionAst node represents the unified conditional branching block, combining a standard "if", "match" and
    "?:" into one.

    Attributes:
        - if_keyword: The "case" keyword.
        - condition: The condition to be evaluated.
        - comp_operator: The optional comparison operator to be used in the condition.
        - then_keyword: The "then" keyword.
        - branches: The pattern blocks to be evaluated.
    """

    if_keyword: "TokenAst"
    condition: "ExpressionAst"
    comp_operator: Optional["TokenAst"]
    then_keyword: "TokenAst"
    branches: List["PatternBlockAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the IfExpressionAst.
        s = ""
        s += f"{self.if_keyword.print(printer)}"
        s += f"{self.condition.print(printer)}"
        s += f" {self.comp_operator.print(printer)}" if self.comp_operator else ""
        s += f" {self.then_keyword.print(printer)}\n"
        s += f"{Seq(self.branches).print(printer, "\n")}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    def infer_type(self, scope_handler, **kwargs) -> InferredType:
        # The IfExpressionAst's returning type is any PatternBlockAst's returning type (all blocks will have the same).
        if self.branches and self.branches[0].body.members:
            return self.branches[0].body.members[-1].infer_type(scope_handler, **kwargs)

        # If there are no statements then every PatternBlockAst is returning the `Void` type.
        return InferredType(convention=ConventionMovAst, type=CommonTypes.void())


__all__ = ["IfExpressionAst"]

from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from SPPCompiler.SemanticAnalysis.ASTs.InnerScopeAst import InnerScopeAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternGuardAst import PatternGuardAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantAst import PatternVariantAst
from SPPCompiler.SemanticAnalysis.ASTs.PatternVariantElseAst import PatternVariantElseAst
from SPPCompiler.SemanticAnalysis.ASTs.StatementAst import StatementAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class PatternBlockAst(Ast, SemanticAnalyser):
    """
    The PatternBlockAst node represents a pattern block. This contains a number of "|" separated patterns, and is used
    to match a value against a pattern.

    Attributes:
        - comp_operator: The optional comparison operator.
        - patterns: The patterns being compared.
        - guard: The optional guard.
        - body: The body of the pattern block.
    """

    comp_operator: Optional[TokenAst]
    patterns: List[PatternVariantAst]
    guard: Optional[PatternGuardAst]
    body: InnerScopeAst[StatementAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternBlockAst.
        s = ""
        s += f"{self.comp_operator.print(printer)} " if self.comp_operator else ""
        s += f"{Seq(self.patterns).print(printer, ", ")}"
        s += f"{self.guard.print(printer)}" if self.guard else ""
        s += f" {self.body.print(printer)}"
        return s

    def is_else_branch(self) -> bool:
        # Helper method to check if the pattern block contains an else branch.
        return isinstance(self.patterns[0], PatternVariantElseAst)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, if_condition: ExpressionAst = None, **kwargs) -> None:
        ...


__all__ = ["PatternBlockAst"]

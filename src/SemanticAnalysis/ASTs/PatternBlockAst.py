from dataclasses import dataclass
from typing import List, Optional

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from src.SemanticAnalysis.ASTs.InnerScopeAst import InnerScopeAst
from src.SemanticAnalysis.ASTs.PatternGuardAst import PatternGuardAst
from src.SemanticAnalysis.ASTs.PatternVariantAst import PatternVariantAst
from src.SemanticAnalysis.ASTs.PatternVariantElseAst import PatternVariantElseAst
from src.SemanticAnalysis.ASTs.StatementAst import StatementAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst

from src.Utils.Sequence import Seq


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
        # Create & enter a new scope for the pattern block.

        scope_handler.into_new_scope(f"<pattern-block:{Seq(self.patterns)}>")

        # Analyse the patterns, guard and body.
        Seq(self.patterns).for_each(lambda p: p.do_semantic_analysis(scope_handler, if_condition, **kwargs))
        self.guard.do_semantic_analysis(scope_handler, **kwargs) if self.guard else None
        self.body.do_semantic_analysis(scope_handler, inline_block=True, **kwargs)

        # Exit the scope.
        scope_handler.exit_cur_scope()


__all__ = ["PatternBlockAst"]

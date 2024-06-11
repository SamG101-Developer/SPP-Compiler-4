from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class PatternBlockAst(Ast, SemanticAnalyser):
    """
    The PatternBlockAst node represents a pattern block. This contains a number of "|" separated patterns, and is used
    to match a value against a pattern.

    Attributes:
        comp_operator: The optional comparison operator.
        patterns: The patterns being compared.
        guard: The optional guard.
        body: The body of the pattern block.
    """

    comp_operator: Optional["TokenAst"]
    patterns: List["PatternVariantAst"]
    guard: Optional["PatternGuardAst"]
    body: "InnerScopeAst[StatementAst]"

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
        from SPPCompiler.SemanticAnalysis.ASTs import PatternVariantElseAst
        return isinstance(self.patterns[0], PatternVariantElseAst)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Each block needs a new scope.
        scope_handler.into_new_scope("<pattern-block>")

        # Analyse the patterns, the guard, and the body of the block.
        Seq(self.patterns).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))
        self.guard.do_semantic_analysis(scope_handler, **kwargs) if self.guard else None
        self.body.do_semantic_analysis(scope_handler, inline=True, **kwargs)

        # Exit the scope.
        scope_handler.exit_cur_scope()


__all__ = ["PatternBlockAst"]

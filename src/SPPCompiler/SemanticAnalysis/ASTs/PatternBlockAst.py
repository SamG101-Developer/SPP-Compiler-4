from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class PatternBlockAst(Ast, SemanticAnalyser):
    """
    The PatternBlockAst node represents a pattern block. This contains a number of "|" separated patterns, and is used
    to match a value against a pattern.

    Attributes:
        comp_operator: The comparison operator.
        patterns: The patterns being compared.
        guard: The optional guard.
        body: The body of the pattern block.
    """

    comp_operator: "TokenAst"
    patterns: List["PatternVariantAst"]
    guard: Optional["PatternGuardAst"]
    body: "InnerScopeAst[StatementAst]"

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst, InnerScopeAst

        # Add a "KwIs" for the "else" pattern, for semantic analysis.
        self.comp_operator = self.comp_operator or TokenAst.dummy(TokenType.KwIs)
        self.body = self.body or InnerScopeAst.default()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternBlockAst.
        s = ""
        s += f"{self.comp_operator.print(printer)}"
        s += f"{Seq(self.patterns).print(printer, ", ")}"
        s += f"{self.guard.print(printer)}" if self.guard else ""
        s += f" {self.body.print(printer)}"
        return s

    def is_else_branch(self) -> bool:
        # Helper method to check if the pattern block contains an else branch.
        from SPPCompiler.SemanticAnalysis.ASTs import PatternVariantElseAst
        return isinstance(self.patterns[0], PatternVariantElseAst)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import PatternVariantElseAst, PatternVariantElseCaseAst

        # Each block needs a new scope.
        scope_handler.into_new_scope("<pattern-block>")

        # Todo: for "is" destructure patterns, a compatibility checker is needed.
        #  - Check the same variables are introduced for each pattern.
        #  - Check if 2+ variant types are being introduced, that no variables are being introduced.
        # print("-" * 100)
        # for p in self.patterns:
        #     non_generative_patterns = (PatternVariantElseAst, PatternVariantElseCaseAst)
        #     if self.comp_operator.token.token_type == TokenType.KwIs and not isinstance(p, non_generative_patterns):
        #         variable = p.convert_to_variable()

        # Analyse the patterns, the guard, and the body of the block.
        Seq(self.patterns).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))
        self.guard.do_semantic_analysis(scope_handler, **kwargs) if self.guard else None
        self.body.do_semantic_analysis(scope_handler, inline=True, **kwargs)

        # Exit the scope.
        scope_handler.exit_cur_scope()


__all__ = ["PatternBlockAst"]

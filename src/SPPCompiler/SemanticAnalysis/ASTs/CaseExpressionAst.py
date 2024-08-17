from dataclasses import dataclass
from typing import List

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class CaseExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The IfExpressionAst node represents the unified conditional branching block, combining a standard "if", "match" and
    "?:" into one.

    Attributes:
        case_keyword: The "case" keyword.
        condition: The condition to be evaluated.
        then_keyword: The "then" keyword.
        branches: The pattern blocks to be evaluated.
    """

    case_keyword: "TokenAst"
    condition: "ExpressionAst"
    then_keyword: "TokenAst"
    branches: List["PatternBlockAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the IfExpressionAst.
        s = ""
        s += f"{self.case_keyword.print(printer)}"
        s += f"{self.condition.print(printer)}"
        s += f" {self.then_keyword.print(printer)}\n"
        s += f"{Seq(self.branches).print(printer, "\n")}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import PatternVariantElseAst, TypeAst, BinaryExpressionAst

        # Move into a new scope.
        scope_handler.into_new_scope("<if-expression>")

        # Analyse the condition.
        self.condition.do_semantic_analysis(scope_handler, **kwargs)

        # Check the branches don't have comparison operators if the condition contains a comparison operator. This is
        # because the condition fragment is combined with the branch fragments, and "1 == == 2" is invalid.
        kwargs |= {"condition": self.condition}
        for branch in self.branches:
            branch.do_semantic_analysis(scope_handler, **kwargs)

            # Check the else branch is the final branch (this also ensures there is only 1 present)
            if isinstance(branch, PatternVariantElseAst) and branch != self.branches[-1]:
                raise SemanticErrors.ELSE_BRANCH_WRONG_POSITION(branch)

            # Combine the condition with the non-"is" branch patterns to ensure functional compatibility.
            if branch.comp_operator.token.token_type != TokenType.KwIs:
                for pattern in branch.patterns:
                    binary_ast = BinaryExpressionAst(branch.pos, self.condition, branch.comp_operator, pattern.expression)
                    binary_ast.do_semantic_analysis(scope_handler, **kwargs)

        if "assignment" in kwargs:
            # If this if-expression is being used for assignment, then all the branches must return the same type.
            branch_return_types = Seq(self.branches).map(lambda b: b.infer_type(scope_handler, **kwargs)).unique_items()
            if branch_return_types.length > 1:
                raise SemanticErrors.CONFLICTING_IF_BRANCH_TYPES(branch_return_types[0], branch_return_types[1])

            # Ensure the final branch is an else branch. todo: exhaustive branches don't need this
            if not isinstance(self.branches[-1], PatternVariantElseAst):
                raise SemanticErrors.NO_ELSE_BRANCH(self.branches[-1])

        # Exit the if-scope.
        scope_handler.exit_cur_scope()

    def infer_type(self, scope_handler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # The IfExpressionAst's returning type is any PatternBlockAst's returning type (all blocks will have the same).
        kwargs |= {"condition": self.condition}
        if self.branches and self.branches[0].body.members:
            return self.branches[0].body.members[-1].infer_type(scope_handler, **kwargs)

        # If there are no statements then every PatternBlockAst is returning the `Void` type.
        return InferredType(convention=ConventionMovAst, type=CommonTypes.void())


__all__ = ["CaseExpressionAst"]

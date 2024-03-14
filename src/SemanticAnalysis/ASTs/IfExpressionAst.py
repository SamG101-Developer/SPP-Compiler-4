from dataclasses import dataclass
from typing import List, Optional, Tuple, Type

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Types.CommonTypes import CommonTypes
from src.SemanticAnalysis.Types.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.PatternVariantElseAst import PatternVariantElseAst

from src.Utils.Sequence import Seq


@dataclass
class IfExpressionAst(Ast, SemanticAnalysis, TypeInfer):
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
        # Move into a new scope for the IfExpressionAst, to contain all the pattern block scopes in 1 place.
        scope_handler.into_new_scope(f"<if-expression:{self.condition}>")

        # Analyse the condition and then each pattern branch
        self.condition.do_semantic_analysis(scope_handler, **kwargs)

        # Check that the branches don't have comparison operators if the if-expression does. This is because the
        # IfExpressionAst's partial-fragment cannot conflict with any of the branches' partial-fragments.
        for branch in self.branches:

            # Analyse the branch
            branch.do_semantic_analysis(scope_handler, if_condition=self.condition, **kwargs)

            # Check there isn't a comparison operator in both partial-fragments.
            if self.comp_operator and branch.comp_operator:
                exception = SemanticError(f"Comparison operators [{self.comp_operator}, {branch.comp_operator}] found in case-expression and pattern block:")
                exception.add_traceback(self.comp_operator.pos, f"1st Comparison operator '{self.comp_operator}' found here.")
                exception.add_traceback(branch.comp_operator.pos, f"2nd Comparison operator '{branch.comp_operator}' found here.")
                raise exception

            # Check there is 1 comparison operator between the IfExpressionAst and the PatternBlockAst.
            if not self.comp_operator and not branch.comp_operator and not isinstance(branch.patterns[0], PatternVariantElseAst):
                exception = SemanticError(f"No comparison operator found in case-expression or pattern block:")
                exception.add_traceback(self.condition.pos, f"Case-expression declared here with no operator")
                exception.add_traceback(branch.body.brace_l_token.pos, f"Branch's pattern block declared here with no operator")
                raise exception

        # TODO: transform the comp_operator into a function call and check its validity.
        # Check that the combination of the case expression and the branch expressions can be combined successfully ie
        # does the complete function exist?
        # for branch in self.branches:
        #     for pattern in branch.patterns:
        #         if isinstance(pattern, PatternVariantElseAst): continue

        # complete_pattern = BinaryExpressionAst(
        #     pos=(self.comp_operator or branch.comp_operator).pos,
        #     lhs=self.condition,
        #     op=(self.comp_operator or branch.comp_operator),
        #     rhs=pattern)
        # TODO: complete_pattern.do_semantic_analysis(scope_handler, **kwargs)
        # TODO: the "rhs" argument is wrong. maybe make a dummy variable of that type?
        # TODO: beneath check too

        # Overriding the comparison classes forces a Bool return type. This check is for the future when member
        # access is implemented too.
        # if (pattern_type := complete_pattern.infer_type(scope_handler, **kwargs))[1] != CommonTypes.bool():
        #     exception = SemanticError(f"Comparisons must evaluate to a boolean expression")
        #     exception.add_traceback(complete_pattern.pos, f"Comparison evaluated here with type '{pattern_type[0]}{pattern_type[1]}'")
        #     raise exception

        # If this "if" expression is being used for assignment, then check that all branches have the same returning
        # type (final statement of the pattern block).
        if kwargs.get("assignment", False):
            if Seq(self.branches).map(lambda b: b.body.members[-1].infer_type(scope_handler, **kwargs)).unique_items().length > 1:
                conflicting_types = Seq(self.branches).map(lambda b: b.body.members[-1].infer_type(scope_handler, **kwargs)).unique_items()
                exception = SemanticError(f"Conflicting types found in if-expression being used for assignment:")
                exception.add_traceback(conflicting_types[0][1].pos, f"Type '{conflicting_types[0][0]}{conflicting_types[0][1]}' inferred here.")  # TODO : wrong.pos
                exception.add_traceback(conflicting_types[1][1].pos, f"Type '{conflicting_types[1][0]}{conflicting_types[1][1]}' inferred here.")  # TODO : wrong.pos
                raise exception

            # Assignment from an "if" expression require an else branch to be present.
            if not self.branches[-1].is_else_branch():
                exception = SemanticError(f"Missing else branch in assign-if-expression:")
                exception.add_traceback(self.pos, f"If-expression declared here.")
                exception.add_traceback(self.branches[-1].pos, f"Last pattern block found here.")
                raise exception

        scope_handler.exit_cur_scope()

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # The IfExpressionAst's returning type is any PatternBlockAst's returning type (all blocks will have the same).
        if self.branches and self.branches[0].body.members:
            return self.branches[0].body.members[-1].infer_type(scope_handler, **kwargs)

        # If there are no statements then every PatternBlockAst is returning the `Void` type.
        return ConventionMovAst, CommonTypes.void()


__all__ = ["IfExpressionAst"]

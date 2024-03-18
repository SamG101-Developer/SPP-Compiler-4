from dataclasses import dataclass

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class PatternGuardAst(Ast, SemanticAnalyser):
    """
    The PatternGuardAst node represents a guard on a conditional branch. This is used to add a condition to a branch's
    pattern, allowing for more precise matching. For example, "case point then == Point(x=0, y) && y > 0" would only
    match "point" if "x" is equal to 0 and "y" is greater than 0.

    Attributes:
        - guard_token: The guard token.
        - expression: The expression being guarded.
    """

    guard_token: TokenAst
    expression: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternGuardAst.
        s = ""
        s += f"{self.guard_token.print(printer)} "
        s += f"{self.expression.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the guard expression.
        self.expression.do_semantic_analysis(scope_handler, **kwargs)

        # Ensure the guard expression is of type "std.Bool".
        expression_type = self.expression.infer_type(scope_handler, **kwargs)[1]
        if not expression_type.symbolic_eq(CommonTypes.bool(), scope_handler.current_scope):
            exception = SemanticError(f"Guard expression must be of type 'Bool':")
            exception.add_traceback(self.expression.pos, f"Expression '{self.expression}' has type '{expression_type}'.")
            raise exception


__all__ = ["PatternGuardAst"]

from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors


@dataclass
class PatternGuardAst(Ast, SemanticAnalyser):
    """
    The PatternGuardAst node represents a guard on a conditional branch. This is used to add a condition to a branch's
    pattern, allowing for more precise matching. For example, "case point then == Point(x=0, y) && y > 0" would only
    match "point" if "x" is equal to 0 and "y" is greater than 0.

    Attributes:
        guard_token: The guard token.
        expression: The expression being guarded.
    """

    guard_token: "TokenAst"
    expression: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PatternGuardAst.
        s = ""
        s += f"{self.guard_token.print(printer)} "
        s += f"{self.expression.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the guard expression
        self.expression.do_semantic_analysis(scope_handler, **kwargs)

        # Ensure the guard expression evaluated to a Bool type.
        if not self.expression.infer_type(scope_handler, **kwargs).type.symbolic_eq(CommonTypes.bool()):
            raise SemanticErrors.TYPE_MISMATCH(self.expression, self.expression.infer_type(scope_handler, **kwargs).type, CommonTypes.bool())


__all__ = ["PatternGuardAst"]

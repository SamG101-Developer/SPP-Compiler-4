from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors


@dataclass
class LoopExpressionConditionBooleanAst(Ast, SemanticAnalyser):
    """
    The LoopExpressionConditionBoolean node is used to represent a condition in a loop expression that must evaluate to
    a boolean value. This is used for "conditional" looping.
    """

    expression: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LoopExpressionConditionBoolean.
        s = ""
        s += f"{self.expression.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # Analyse the expression.
        loop_owner = kwargs.pop("loop-owner")
        self.expression.do_semantic_analysis(scope_handler, **kwargs)

        # Ensure the expression evaluates to a Bool type.
        condition_type = self.expression.infer_type(scope_handler, **kwargs)
        target_type = InferredType(convention=ConventionMovAst, type=CommonTypes.bool(self.expression.pos))
        if not condition_type.symbolic_eq(target_type, scope_handler.current_scope):
            raise SemanticErrors.CONDITION_NON_BOOLEAN(loop_owner, self.expression, condition_type, "loop")

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # The condition's type is always "Bool" (analysis ensures this).
        return InferredType(convention=ConventionMovAst, type=CommonTypes.bool(pos=self.pos))


__all__ = ["LoopExpressionConditionBooleanAst"]

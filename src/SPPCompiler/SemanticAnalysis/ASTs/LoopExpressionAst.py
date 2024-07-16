from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors


@dataclass
class LoopExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The LoopExpressionAst node is used to represent a while-loop. This is a loop that will continue to execute the body
    of the loop while the condition is true. The keyword to use it is "loop".

    Attributes:
        loop_keyword: The "loop" keyword.
        condition: The condition to check each iteration.
        body: The body of the loop.
        else_block: The optional else block of the loop.
    """

    loop_keyword: "TokenAst"
    condition: "LoopExpressionConditionAst"
    body: "InnerScopeAst[StatementAst]"
    else_block: Optional["WhileElseExpressionAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the WhileExpressionAst.
        s = ""
        s += f"{self.loop_keyword.print(printer)}"
        s += f"{self.condition.print(printer)} "
        s += f"{self.body.print(printer)}"
        s += f"{self.else_block.print(printer)}" if self.else_block else ""
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst

        # Analyse the condition
        if isinstance(self.condition, TypeAst):
            raise SemanticErrors.INVALID_USE_OF_TYPE_AS_EXPR(self.condition)
        self.condition.do_semantic_analysis(scope_handler, **kwargs)

        kwargs["loop-count"] = kwargs.get("loop-count", 0) + 1
        kwargs["loop-types"] = kwargs.get("loop-types", {})
        self.body.do_semantic_analysis(scope_handler, **kwargs)
        kwargs["loop-count"] -= 1

        if self.else_block:
            self.else_block.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

        # The resulting type of a loop is always "Void", because it never returns (dummy value).
        return InferredType(convention=ConventionMovAst, type=CommonTypes.void(pos=self.pos))


__all__ = ["LoopExpressionAst"]

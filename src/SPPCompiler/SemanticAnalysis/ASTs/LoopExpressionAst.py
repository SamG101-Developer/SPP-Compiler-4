from dataclasses import dataclass, field
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

    _loop_type_info: dict = field(default_factory=dict, init=False, repr=False)
    _loop_type_index: int = field(default=0, init=False, repr=False)

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
        # Analyse the condition
        self.condition.do_semantic_analysis(scope_handler, **(kwargs | {"loop-owner": self}))

        scope_handler.into_new_scope("<loop-block>")

        for i in range(("loop-count" not in kwargs) + 1):
            kwargs["loop-count"] = kwargs.get("loop-count", 0) + 1
            kwargs["loop-types"] = kwargs.get("loop-types", {})
            self._loop_type_info = kwargs["loop-types"]
            self._loop_type_index = kwargs["loop-count"]
            self.body.do_semantic_analysis(scope_handler, **kwargs)
            kwargs["loop-count"] -= 1

        scope_handler.exit_cur_scope()

        if self.else_block:
            self.else_block.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # Get the type from the dictionary, set by the do_semantic_analysis method.
        loop_type = self._loop_type_info.get(self._loop_type_index, (None, None))[1]
        if not loop_type:
            loop_type = InferredType(convention=ConventionMovAst, type=CommonTypes.void(self.pos))

        return loop_type


__all__ = ["LoopExpressionAst"]

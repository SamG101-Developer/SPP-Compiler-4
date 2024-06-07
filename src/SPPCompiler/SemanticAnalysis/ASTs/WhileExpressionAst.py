from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class WhileExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The WhileExpressionAst node is used to represent a while-loop. This is a loop that will continue to execute the body
    of the loop while the condition is true. The keyword to use it is "loop".

    Attributes:
        - while_keyword: The "loop" keyword.
        - condition: The condition to check each iteration.
        - body: The body of the loop.
        - else_block: The optional else block of the loop.
    """

    while_keyword: "TokenAst"
    condition: "ExpressionAst"
    body: "InnerScopeAst[StatementAst]"
    else_block: Optional["WhileElseExpressionAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the WhileExpressionAst.
        s = ""
        s += f"{self.while_keyword.print(printer)}"
        s += f"{self.condition.print(printer)} "
        s += f"{self.body.print(printer)}"
        s += f"{self.else_block.print(printer)}" if self.else_block else ""
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

        # The resulting type of a loop is always "Void", because it never returns (dummy value).
        return InferredType(convention=ConventionMovAst, type=CommonTypes.void(pos=self.pos))


__all__ = ["WhileExpressionAst"]

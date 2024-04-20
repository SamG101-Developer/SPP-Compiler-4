from dataclasses import dataclass
from typing import Optional, Tuple, Type

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst


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
        # Analyse the condition and ensure it is a Boolean condition.
        self.condition.do_semantic_analysis(scope_handler, **kwargs)
        condition_type = self.condition.infer_type(scope_handler, **kwargs)
        if not condition_type[1].symbolic_eq(CommonTypes.bool(), scope_handler.current_scope):
            raise SemanticError().add_error(
                pos=self.condition.pos,
                error_type=SemanticErrorType.TYPE_ERROR,
                message=f"Guard expression must be of type 'Bool':",
                tag_message=f"Type inferred as '{condition_type[0]}{condition_type[1]}'.",
                tip="Use a boolean expression for the guard.")

        # Analyse the body
        self.body.do_semantic_analysis(scope_handler, **kwargs)

        # Analyse the else block is it exists.
        if self.else_block:
            self.else_block.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # The resulting type of a loop is always "Void", because it never returns (dummy value).
        return ConventionMovAst, CommonTypes.void(pos=self.pos)


__all__ = ["WhileExpressionAst"]

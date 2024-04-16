from dataclasses import dataclass
from typing import Tuple, Type

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.PostfixExpressionAst import PostfixExpressionAst
from src.SemanticAnalysis.ASTs.PostfixExpressionOperatorFunctionCallAst import PostfixExpressionOperatorFunctionCallAst


@dataclass
class UnaryExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The UnaryExpressionAst node represents a unary expression. This is an expression that has a unary operator and a
    right-hand side expression. As S++ doesn't have unary expressions, the only unary operator is the 'async' function
    call prefix.

    Attributes:
        - op: The unary operator.
        - rhs: The right-hand side expression.
    """

    op: "UnaryOperatorAst"
    rhs: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the UnaryExpressionAst.
        s = ""
        s += f"{self.op.print(printer)}"
        s += f"{self.rhs.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check that the rhs is a function call (only unary is async)
        if not (isinstance(self.rhs, PostfixExpressionAst) and isinstance(self.rhs.op, PostfixExpressionOperatorFunctionCallAst)):
            exception = SemanticError(f"Invalid 'async' usage:")
            exception.add_error(self.pos, f"'{self}' is not a function call.")
            raise exception

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # The type is a Fut[T] where T is the return type of the function call.
        # TODO: this will cause an error, because the RHS hasn't been analysed yet
        return ConventionMovAst, CommonTypes.fut(self.rhs.infer_type(scope_handler, **kwargs)[1], pos=self.pos)


__all__ = ["UnaryExpressionAst"]

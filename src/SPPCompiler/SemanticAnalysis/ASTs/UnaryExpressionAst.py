from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors


@dataclass
class UnaryExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The UnaryExpressionAst node represents a unary expression. This is an expression that has a unary operator and a
    right-hand side expression. As S++ doesn't have unary expressions, the only unary operator is the 'async' function
    call prefix.

    Attributes:
        op: The unary operator.
        rhs: The right-hand side expression.
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
        from SPPCompiler.SemanticAnalysis.ASTs import PostfixExpressionAst, PostfixExpressionOperatorFunctionCallAst

        # This is only used for the "async" function calls, so ensure that the RHS is a function call.
        if not isinstance(self.rhs, PostfixExpressionAst) or not isinstance(self.rhs.op, PostfixExpressionOperatorFunctionCallAst):
            raise SemanticErrors.INVALID_ASYNC_CALL(self, self.rhs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

        # The type is a Fut[T] where T is the return type of the function call.
        # TODO: this will cause an error, because the RHS hasn't been analysed yet
        future_type = CommonTypes.fut(self.rhs.infer_type(scope_handler, **kwargs)[1], pos=self.pos)
        return InferredType(
            convention=ConventionMovAst,
            type_symbol=scope_handler.current_scope.get_symbol(future_type))


__all__ = ["UnaryExpressionAst"]

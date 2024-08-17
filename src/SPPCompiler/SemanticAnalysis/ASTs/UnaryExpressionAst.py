from dataclasses import dataclass

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors


@dataclass
class UnaryExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
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

        # Ensure that the LHS is the "async" keyword.
        if not self.op.token.token_type == TokenType.KwAsync:
            raise SemanticErrors.INVALID_ASYNC_CALL(self, self.op)  # todo

        # This is only used for the "async" function calls, so ensure that the RHS is a function call.
        if not isinstance(self.rhs, PostfixExpressionAst) or not isinstance(self.rhs.op, PostfixExpressionOperatorFunctionCallAst):
            raise SemanticErrors.INVALID_ASYNC_CALL(self, self.rhs)

        # Mark the function call as async and analyse it for type inference.
        self.rhs.op._is_async = self.op
        self.rhs.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # The type is a Fut[T] where T is the return type of the function call.
        future_type = CommonTypes.fut(self.rhs.infer_type(scope_handler, **kwargs).type, pos=self.pos)
        future_type.do_semantic_analysis(scope_handler, **kwargs)
        return InferredType(convention=ConventionMovAst, type=future_type)


__all__ = ["UnaryExpressionAst"]

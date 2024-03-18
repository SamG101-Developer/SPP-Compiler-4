from dataclasses import dataclass
from typing import Tuple, Type

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class PostfixExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The PostfixExpressionAst node represents a postfix expression. This combines the expression on the LHS with the
    postfix operator on the RHS (function call, member access, etc.).

    Attributes:
        - lhs: The expression on the LHS.
        - op: The postfix operator on the RHS.
    """

    lhs: "ExpressionAst"
    op: "PostfixExpressionOperatorAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PostfixExpressionAst.
        s = ""
        s += f"{self.lhs.print(printer)}"
        s += f"{self.op.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the LHS and the operator.
        self.lhs.do_semantic_analysis(scope_handler, **kwargs)
        self.op.do_semantic_analysis(scope_handler, lhs=self.lhs, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # Infer the type of the operator applied over the LHS.
        return self.op.infer_type(scope_handler, lhs=self.lhs, **kwargs)

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same lhs and operator.
        return isinstance(other, PostfixExpressionAst) and self.lhs == other.lhs and self.op == other.op

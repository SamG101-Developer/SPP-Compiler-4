from dataclasses import dataclass

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import NamespaceSymbol, VariableSymbol


@dataclass
class PostfixExpressionOperatorNotKeywordAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The PostfixExpressionOperatorNotKeywordAst node represents the "not" keyword operator of some expression.

    Attributes:
        dot_token: The dot token that represents the member access operator.
        not_token: The not token that represents the "not" keyword operator.
    """

    dot_token: "TokenAst"
    not_token: "TokenAst"

    def print(self, printer: AstPrinter) -> str:
        # Print the "not" keyword operator.
        s = ""
        s += f"{self.dot_token.print(printer)}"
        s += f"{self.not_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, lhs: "ExpressionAst" = None, **kwargs) -> None:
        # Ensure the LHS is a boolean type.
        lhs_type = lhs.infer_type(scope_handler, **kwargs).type
        if not lhs_type.symbolic_eq(CommonTypes.bool(), scope_handler.current_scope):
            raise SemanticErrors.CONDITION_NON_BOOLEAN(self.not_token, lhs, lhs_type, "not")

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # The "not" keyword operator always returns a boolean.
        return InferredType(convention=ConventionMovAst, type=CommonTypes.bool())

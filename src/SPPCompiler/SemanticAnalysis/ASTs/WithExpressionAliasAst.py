from dataclasses import dataclass
from typing import Optional

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler, Scope
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class WithExpressionAliasAst(Ast, SemanticAnalyser):
    """
    The WithExpressionAliasAst node represents the aliasing of a variable in a "with" expression. This allows for an
    expression to be evaluated into a variable, and then the ".enter()" method called on the variable. Any type of local
    variable, such as object destructuring etc can be used here.

    Attributes:
        variable: The variable to be assigned to.
        assign_token: The assignment token.
    """

    variable: "LocalVariableAst"
    assign_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the WithExpressionAliasAst.
        s = ""
        s += f"{self.variable.print(printer)}"
        s += f"{self.assign_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, with_expression: "ExpressionAst" = None, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import (
            LetStatementInitializedAst, TokenAst, PostfixExpressionAst, PostfixExpressionOperatorMemberAccessAst,
            PostfixExpressionOperatorFunctionCallAst, IdentifierAst)

        # Create the ".enter()" function call to get the contextual entering object.
        ast_0 = PostfixExpressionOperatorMemberAccessAst(pos=self.variable.pos, dot_token=TokenAst.dummy(TokenType.TkDot), identifier=IdentifierAst(self.variable.pos, "enter"))
        ast_1 = PostfixExpressionAst(pos=self.variable.pos, lhs=with_expression, op=ast_0)
        ast_2 = PostfixExpressionOperatorFunctionCallAst(pos=self.variable.pos, generic_arguments=None, arguments=None, fold_token=None)
        ast_3 = PostfixExpressionAst(pos=self.variable.pos, lhs=ast_1, op=ast_2)

        # Convert the variable assignment into a "let" statement.
        let_statement = LetStatementInitializedAst(
            pos=self.variable.pos,
            let_keyword=TokenAst.dummy(TokenType.KwLet),
            assign_to=self.variable,
            assign_token=self.assign_token,
            value=ast_3)

        # Do semantic analysis on it, handling symbol generation, etc.
        let_statement.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["WithExpressionAliasAst"]

from dataclasses import dataclass

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from src.SemanticAnalysis.ASTs.LetStatementInitializedAst import LetStatementInitializedAst
from src.SemanticAnalysis.ASTs.LocalVariableAst import LocalVariableAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class WithExpressionAliasAst(Ast, SemanticAnalysis):
    """
    The WithExpressionAliasAst node represents the aliasing of a variable in a "with" expression. This allows for an
    expression to be evaluated into a variable, and then the ".enter()" method called on the variable. Any type of local
    variable, such as object destructuring etc can be used here.

    Attributes:
        - variable: The variable to be assigned to.
        - assign_token: The assignment token.
    """

    variable: LocalVariableAst
    assign_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the WithExpressionAliasAst.
        s = ""
        s += f"{self.variable.print(printer)}"
        s += f"{self.assign_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, with_expression: ExpressionAst = None, **kwargs) -> None:
        # Convert the variable assignment into a "let" statement.
        let_statement = LetStatementInitializedAst(
            pos=self.variable.pos,
            let_keyword=TokenAst.dummy(TokenType.KwLet),
            assign_to=self.variable,
            assign_token=self.assign_token,
            value=with_expression)

        # Do semantic analysis on it, handling symbol generation, etc.
        let_statement.do_semantic_analysis(scope_handler, **kwargs)

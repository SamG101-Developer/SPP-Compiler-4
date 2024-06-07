from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class LocalVariableTupleAst(Ast, SemanticAnalyser):
    """
    The LocalVariableTupleAst node represents a tuple of local variables. This is an advanced form of a local variable,
    and is seen mostly in the "let" statement. For example, in the statement "let (mut x, y) = (5, 6)", "(mut x, y)" is
    the tuple of local variables. Both "mut x" and "y" are separate single local variables.

    Attributes:
        paren_l_token: The left parenthesis token.
        items: The local variables in the tuple.
        paren_r_token: The right parenthesis token.
    """

    paren_l_token: "TokenAst"
    items: List["LocalVariableSingleAst"]
    paren_r_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LocalVariableTupleAst.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.items).print(printer, ", ")}"
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, other_tuple: "ExpressionAst" = None, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import (
            LocalVariableSkipArgumentAst, PostfixExpressionOperatorMemberAccessAst, PostfixExpressionAst, TokenAst,
            LetStatementInitializedAst)
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType

        # Only allow 1 multi-skip inside a tuple.
        skips = Seq(self.items).filter_to_type(LocalVariableSkipArgumentAst)
        if skips.length > 1:
            exception = SemanticError()
            exception.add_info(
                pos=skips[0].pos, tag_message="First skip found here.")
            exception.add_error(
                pos=skips[1].pos, error_type=SemanticErrorType.ORDER_ERROR,
                tag_message="Second skip found here.",
                message="Only one skip argument is allowed in a tuple.",
                tip="Remove the additional skip argument.")
            raise exception

        # Ensure that the tuple has the same number of items as the other tuple.
        lhs_tuple_elements = self.items
        rhs_tuple_elements = other_tuple.infer_type(scope_handler, **kwargs)[1].parts[-1].generic_arguments.arguments
        if len(lhs_tuple_elements) != len(rhs_tuple_elements):
            exception = SemanticError()
            exception.add_info(
                pos=self.pos, tag_message=f"Assignment tuple contains {len(lhs_tuple_elements)} items.")
            exception.add_error(
                pos=self.pos, error_type=SemanticErrorType.TYPE_ERROR,
                tag_message=f"Assignment value contains {len(rhs_tuple_elements)} items.",
                message="The length of the tuple does not match the length of the other tuple.",
                tip="Ensure that the tuple has the same number of items as the other tuple.")
            raise exception

        # Create new "let" statements for each element of the tuple.
        new_let_statements = []
        cur_let_statements = Seq(self.items).filter_not_type(LocalVariableSkipArgumentAst)
        for i, current_local_variable in cur_let_statements.enumerate():
            ast_0 = PostfixExpressionOperatorMemberAccessAst(
                pos=self.pos,
                dot_token=TokenAst.dummy(TokenType.TkDot),
                identifier=TokenAst.dummy(TokenType.LxDecDigits, info=f"{i}"))

            ast_1 = PostfixExpressionAst(
                pos=self.pos,
                lhs=kwargs["value"],
                op=ast_0)

            ast_2 = LetStatementInitializedAst(
                pos=self.pos,
                let_keyword=TokenAst.dummy(TokenType.KwLet),
                assign_to=current_local_variable,
                assign_token=TokenAst.dummy(TokenType.TkAssign, pos=self.pos),
                value=ast_1)

        # Analyse the new "let" statements, which may contain nested tuples/types themselves.
        for new_let_statement in new_let_statements:
            new_let_statement.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["LocalVariableTupleAst"]

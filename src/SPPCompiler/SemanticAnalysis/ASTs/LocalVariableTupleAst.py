from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
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

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import (
            LocalVariableSkipArgumentAst, PostfixExpressionOperatorMemberAccessAst, PostfixExpressionAst, TokenAst,
            LetStatementInitializedAst)
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType

        # Only allow 1 multi-skip inside a tuple.
        skips = Seq(self.items).filter_to_type(LocalVariableSkipArgumentAst)
        if skips.length > 1:
            raise SemanticErrors.MULTIPLE_ARGUMENT_SKIPS(skips[0], skips[1])

        # Ensure that the tuple has the same number of items as the other tuple.
        lhs_tuple_elements = self.items
        rhs_tuple_elements = kwargs["value"].infer_type(scope_handler, **kwargs).type.parts[-1].generic_arguments.arguments
        if len(lhs_tuple_elements) < len(rhs_tuple_elements) and not skips:
            raise SemanticErrors.TUPLE_SIZE_MISMATCH(self, kwargs["value"], len(lhs_tuple_elements), len(rhs_tuple_elements))

        # Create new "let" statements for each element of the tuple.
        new_let_statements = []
        cur_let_statements = Seq(self.items).filter_not_type(LocalVariableSkipArgumentAst)
        for i, current_local_variable in cur_let_statements.enumerate():
            ast_0 = PostfixExpressionOperatorMemberAccessAst(
                pos=self.pos,
                dot_token=TokenAst.dummy(TokenType.TkDot),
                identifier=TokenAst.dummy(TokenType.LxDecInteger, info=f"{i}"))

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

            new_let_statements.append(ast_2)

        # Analyse the new "let" statements, which may contain nested tuples/types themselves.
        for new_let_statement in new_let_statements:
            new_let_statement.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["LocalVariableTupleAst"]

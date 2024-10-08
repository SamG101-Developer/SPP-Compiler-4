from dataclasses import dataclass
from typing import List

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class LocalVariableTupleDestructureAst(Ast, SemanticAnalyser):
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
            LocalVariableSkipArgumentAst, LocalVariableSkipArgumentsAst, PostfixExpressionOperatorMemberAccessAst,
            PostfixExpressionAst, TokenAst, LetStatementInitializedAst, TupleLiteralAst)

        # Analyse the value.
        value = kwargs["value"]
        value.do_semantic_analysis(scope_handler, **kwargs)

        # Only allow 1 multi-skip inside a tuple.
        skips = Seq(self.items).filter_to_type(LocalVariableSkipArgumentsAst)
        if skips.length > 1:
            raise SemanticErrors.MULTIPLE_ARGUMENT_SKIPS(skips[0], skips[1])

        # Ensure that the tuple has the same number of items as the other tuple.
        lhs_tuple_elements = self.items
        rhs_tuple_elements = value.infer_type(scope_handler, **kwargs).type.types[-1].generic_arguments.arguments
        # print(value, Seq(self.items), len(lhs_tuple_elements), len(rhs_tuple_elements))
        if len(lhs_tuple_elements) < len(rhs_tuple_elements) and not skips:
            raise SemanticErrors.TUPLE_SIZE_MISMATCH(self, value, len(lhs_tuple_elements), len(rhs_tuple_elements))

        # Create new "let" statements for each element of the tuple.

        items = self.items.copy()
        for i, current_local_variable in Seq(self.items).enumerate():
            if isinstance(current_local_variable, LocalVariableSkipArgumentsAst) and not rhs_tuple_elements:
                raise SemanticErrors.SKIPPING_ARGUMENTS_IN_STATELESS_TYPE(current_local_variable)
            elif isinstance(current_local_variable, LocalVariableSkipArgumentsAst):
                number_elements_skipped = len(rhs_tuple_elements) - len(lhs_tuple_elements) + 1
                current_local_variable._num_skipped = number_elements_skipped
                dummy_replacements = [LocalVariableSkipArgumentAst(current_local_variable.pos, TokenAst.dummy(TokenType.TkUnderscore))] * (number_elements_skipped - 1)
                items = items[:i] + dummy_replacements + items[i:]
                break

        for i, current_local_variable in Seq(items).enumerate():
            if isinstance(current_local_variable, LocalVariableSkipArgumentAst):
                continue
            elif isinstance(current_local_variable, LocalVariableSkipArgumentsAst) and current_local_variable.binding:

                nested_asts = []
                for j in range(current_local_variable._num_skipped):
                    ast_0 = PostfixExpressionOperatorMemberAccessAst(self.pos, TokenAst.dummy(TokenType.TkDot), TokenAst.dummy(TokenType.LxDecInteger, info=f"{i - current_local_variable._num_skipped + j + 1}"))
                    ast_1 = PostfixExpressionAst(self.pos, value, ast_0)
                    nested_asts.append(ast_1)

                ast_0 = TupleLiteralAst(self.pos, TokenAst.dummy(TokenType.TkParenL), nested_asts, TokenAst.dummy(TokenType.TkParenR))
                ast_1 = LetStatementInitializedAst(self.pos, TokenAst.dummy(TokenType.KwLet), current_local_variable.binding, TokenAst.dummy(TokenType.TkAssign, pos=self.pos), ast_0)
                ast_1.do_semantic_analysis(scope_handler, **kwargs)
                continue

            elif isinstance(current_local_variable, LocalVariableSkipArgumentsAst):
                continue

            ast_0 = PostfixExpressionOperatorMemberAccessAst(self.pos, TokenAst.dummy(TokenType.TkDot), TokenAst.dummy(TokenType.LxDecInteger, info=f"{i}"))
            ast_1 = PostfixExpressionAst(self.pos, value, ast_0)
            ast_2 = LetStatementInitializedAst(self.pos, TokenAst.dummy(TokenType.KwLet), current_local_variable, TokenAst.dummy(TokenType.TkAssign, pos=self.pos), ast_1)
            ast_2.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["LocalVariableTupleDestructureAst"]

from dataclasses import dataclass, field
from typing import Optional, Tuple, Type

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser, BIN_OP_FUNCS, OP_PREC
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from src.SemanticAnalysis.Utils.SemanticError import SemanticError
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from src.SemanticAnalysis.ASTs.FunctionArgumentGroupAst import FunctionArgumentGroupAst
from src.SemanticAnalysis.ASTs.FunctionArgumentNormalAst import FunctionArgumentNormalAst
from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.PostfixExpressionAst import PostfixExpressionAst
from src.SemanticAnalysis.ASTs.PostfixExpressionOperatorMemberAccessAst import PostfixExpressionOperatorMemberAccessAst
from src.SemanticAnalysis.ASTs.PostfixExpressionOperatorFunctionCallAst import PostfixExpressionOperatorFunctionCallAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class BinaryExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The BinaryExpressionAst node is used to represent a binary expression, such as "a + b" or "a < b". The binary
    expression has a left-hand-side, a right-hand-side, and an operator.

    Attributes:
        - lhs: The left-hand-side of the binary expression.
        - op: The operator of the binary expression.
        - rhs: The right-hand-side of the binary expression.

        - _as_func: The function transformation of the binary expression (a + b => a.add(b)).
    """

    lhs: "ExpressionAst"
    op: "TokenAst"
    rhs: "ExpressionAst"
    _as_func: Optional["PostfixExpressionAst"] = field(default=None, init=False)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the BinaryExpressionAst
        s = ""
        s += f"{self.lhs.print(printer)} "
        s += f"{self.op.print(printer)} "
        s += f"{self.rhs.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # TODO : special cases for ?? and "is".
        # TODO : special case for ".." as an operand

        # 3 stage mutation operation
        #   1. Re-arrange the arguments in the binary expression. To mitigate left-hand parsing issues, right hand parsing was used.
        #   2. Chain any comparison operators together, so that "a < b < c" becomes "a < b && b < c".
        #   3. Transform the binary expression to a function call.

        binary_l_folding = False
        binary_r_folding = False

        # Handle "tuple_variable + .." tuple binary right-folding.
        if isinstance(self.rhs, TokenAst):
            assert self.rhs.token.token_type == TokenType.TkVariadic
            self.lhs, self.rhs = self.rhs, self.lhs
            binary_r_folding = True

        # Handle ".. + tuple_variable" tuple binary left-folding.
        if isinstance(self.lhs, TokenAst):
            assert self.lhs.token.token_type == TokenType.TkVariadic
            binary_l_folding = True

            # Ensure the RHS is a tuple type.
            rhs_type = self.rhs.infer_type(scope_handler, **kwargs)
            if rhs_type[0] != ConventionMovAst or not rhs_type[1].without_generics().symbolic_eq(CommonTypes.tuple([]), scope_handler.current_scope):
                exception = SemanticError("Can only binary-fold a tuple type")
                exception.add_traceback(rhs_type[1].pos, f"Inferred RHS type as '{rhs_type[0]}{rhs_type[1]}'")
                raise exception

            # Ensure tuple's items' types are all the same.
            tuple_item_types = rhs_type[1].parts[-1].generic_arguments.arguments
            if different_type := next((t.type for t in tuple_item_types if not tuple_item_types[0].type.symbolic_eq(t.type, scope_handler.current_scope)), None):
                exception = SemanticError("Can only binary-fold a tuple type with all the same types")
                exception.add_traceback(rhs_type[1].pos, f"Inferred RHS type as '{rhs_type[0]}{rhs_type[1]}'")
                exception.add_traceback_minimal(tuple_item_types[0].type.pos, f"First item type in tuple inferred as '{tuple_item_types[0]}'")
                exception.add_traceback_minimal(different_type.pos, f"Different item type in tuple inferred as '{different_type}'")
                raise exception

            # Ensure the tuple has at least 2 items.
            if len(tuple_item_types) < 2:
                exception = SemanticError("Can only binary-fold a tuple type with at least 2 items")
                exception.add_traceback(rhs_type[1].pos, f"Inferred RHS type as '{rhs_type[0]}{rhs_type[1]}'")
                raise exception

            # Alter the LHS and RHS for type-based analysis.
            from src.LexicalAnalysis.Lexer import Lexer
            from src.SyntacticAnalysis.Parser import Parser
            self.lhs = Parser(Lexer(f"{self.rhs}.0").lex(), "temp").parse_expression().parse_once()
            self.rhs = Parser(Lexer(f"{self.rhs}.1").lex(), "temp").parse_expression().parse_once()

        if binary_r_folding:
            self.lhs, self.rhs = self.rhs, self.lhs

        ast = BinaryExpressionAstUtils.fix_associativity(self)
        ast = BinaryExpressionAstUtils.combine_comparison_operators(ast)
        ast = BinaryExpressionAstUtils.convert_all_to_function(ast)

        self._as_func = ast
        self._as_func.do_semantic_analysis(scope_handler, **kwargs)
        return self._as_func

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # TODO : special case for ".." as an operand
        return self._as_func.infer_type(scope_handler, **kwargs)


class BinaryExpressionAstUtils:
    @staticmethod
    def fix_associativity(ast: BinaryExpressionAst) -> BinaryExpressionAst:
        # To mitigate the issue of left-hand recursive parsing, the parser uses right-hand recursive parsing. This
        # puts equal precedence operators on the right hand side of the tree, ie 1 + 2 + 3 would be 1 + (2 + 3), not
        # (1 + 2) + 3. This function fixes the associativity of the tree, so that the left hand side is always the
        # first operand, and the right hand side is always the last operand. TODO : this is bugged
        if not isinstance(ast.rhs, BinaryExpressionAst):
            return ast

        if OP_PREC[ast.op.token.token_type] >= OP_PREC[ast.rhs.op.token.token_type]:
            rhs = ast.rhs
            ast.rhs = rhs.rhs
            rhs.rhs = rhs.lhs
            rhs.lhs = ast.lhs
            rhs.op, ast.op = ast.op, rhs.op
            ast.lhs = rhs
            return BinaryExpressionAstUtils.fix_associativity(ast)

        return ast

    @staticmethod
    def combine_comparison_operators(ast: BinaryExpressionAst) -> BinaryExpressionAst:
        # A Python-borrowed feature is the combination of comparison operators, such as "a < b < c". This function
        # recursively combines comparison operators into a single binary expression, so that "a < b < c" becomes
        # "a < b && b < c".

        def is_comparison_operator(token):
            return token in {TokenType.TkEq, TokenType.TkNe, TokenType.TkLt, TokenType.TkGt, TokenType.TkLe, TokenType.TkGe}

        # If the ast isn't a binary expression, or the binary operator is not comparable-combinable, then return the
        # current AST, whatever it is.
        if not isinstance(ast.lhs, BinaryExpressionAst) or not is_comparison_operator(ast.op.token.token_type):
            return ast

        # Combine the comparison into separate binary expressions, so that "a < b < c" becomes "a < b && b < c".
        lhs = ast.lhs
        rhs = ast.rhs
        lhs = lhs.rhs
        ast.rhs = BinaryExpressionAst(ast.pos, lhs, ast.op, rhs)
        ast.op  = TokenAst.dummy(TokenType.TkLogicalAnd)
        BinaryExpressionAstUtils.combine_comparison_operators(ast.lhs)

        return ast

    @staticmethod
    def convert_to_function(ast: BinaryExpressionAst) -> PostfixExpressionAst:
        # Transform the binary expression to a function call. This doesn't have to go in pre-processing, because the
        # transformation is only temporary, and doesn't affect the tree at all. The original binary expression is
        # still used for the rest of the semantic analysis.

        # For "a + b", this would be "a.add"
        mock_function = PostfixExpressionAst(
            pos=ast.pos,
            lhs=ast.lhs,
            op=PostfixExpressionOperatorMemberAccessAst(
                pos=ast.op.pos,
                dot_token=TokenAst.dummy(TokenType.TkDot),
                identifier=IdentifierAst(ast.op.pos, BIN_OP_FUNCS[ast.op.token.token_type])))

        # For "a + b", this would be "b"
        mock_function_call_argument = FunctionArgumentNormalAst(
            pos=ast.op.pos,
            convention=ConventionMovAst(ast.rhs.pos),
            unpack_token=None,
            value=ast.rhs)

        # For "a + b", this would be "a.add(b)"
        mock_function_call = PostfixExpressionAst(
            pos=ast.pos,
            lhs=mock_function,
            op=PostfixExpressionOperatorFunctionCallAst(
                pos=ast.op.pos,
                arguments=FunctionArgumentGroupAst(
                    pos=ast.op.pos,
                    paren_l_token=TokenAst.dummy(TokenType.TkParenL),
                    arguments=[mock_function_call_argument],
                    paren_r_token=TokenAst.dummy(TokenType.TkParenR)),
                generic_arguments=None,
                fold_token=None))

        return mock_function_call

    @staticmethod
    def convert_all_to_function(ast: BinaryExpressionAst) -> PostfixExpressionAst:
        if not isinstance(ast, BinaryExpressionAst):
            return ast

        ast.lhs = BinaryExpressionAstUtils.convert_all_to_function(ast.lhs)
        ast.rhs = BinaryExpressionAstUtils.convert_all_to_function(ast.rhs)
        return BinaryExpressionAstUtils.convert_to_function(ast)


__all__ = ["BinaryExpressionAst"]

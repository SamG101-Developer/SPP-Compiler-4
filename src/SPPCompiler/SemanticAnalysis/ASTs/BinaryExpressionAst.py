from dataclasses import dataclass, field
from typing import Optional

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstOperators import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class BinaryExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The BinaryExpressionAst node is used to represent a binary expression, such as "a + b" or "a < b". The binary
    expression has a left-hand-side, a right-hand-side, and an operator.

    Attributes:
        lhs: The left-hand-side of the binary expression.
        op: The operator of the binary expression.
        rhs: The right-hand-side of the binary expression.

        _as_func: The function transformation of the binary expression (a + b => a.add(b)).
    """

    lhs: "ExpressionAst"
    op: "TokenAst"
    rhs: "ExpressionAst"
    _as_func: Optional["PostfixExpressionAst"] = field(default=None, init=False)
    _bin_fold: bool = field(default=False, init=False)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst
        self._bin_fold = any([
            isinstance(self.lhs, TokenAst) and self.lhs.token.token_type == TokenType.TkVariadic,
            isinstance(self.rhs, TokenAst) and self.rhs.token.token_type == TokenType.TkVariadic])

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the BinaryExpressionAst
        s = ""
        s += f"{self.lhs.print(printer)} "
        s += f"{self.op.print(printer)} "
        s += f"{self.rhs.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst
        from SPPCompiler.LexicalAnalysis.Lexer import Lexer
        from SPPCompiler.SyntacticAnalysis.Parser import Parser
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

        is_binary_foldl = False
        is_binary_foldr = False

        # Handle "tuple + .." binary right-folding.
        if isinstance(self.rhs, TokenAst):
            assert self.rhs.token.token_type == TokenType.TkVariadic
            self.lhs, self.rhs = self.rhs, self.lhs
            is_binary_foldr = True

        # Handle ".. + tuple" binary left-folding.
        if isinstance(self.lhs, TokenAst):
            assert self.lhs.token.token_type == TokenType.TkVariadic
            is_binary_foldl = True

            # Ensure the RHS is an owned tuple-type.
            rhs_type = self.rhs.infer_type(scope_handler, **kwargs)
            is_tuple_type = rhs_type.type.without_generics().symbolic_eq(CommonTypes.tuple([]), scope_handler.current_scope)
            if rhs_type.convention != ConventionMovAst or not is_tuple_type:
                raise SemanticErrors.INVALID_BINARY_FOLD_EXPR_TYPE(self.rhs, rhs_type)

            # Ensure the RHS tuple elements are all the same type.
            tuple_element_types = Seq(rhs_type.type.parts[-1].generic_arguments.arguments)
            if not tuple_element_types.all(lambda t: t.type.symbolic_eq(tuple_element_types[0].type, scope_handler.current_scope)):
                raise SemanticErrors.INVALID_BINARY_FOLD_EXPR_ELEMENT_TYPE(self.rhs, rhs_type)

            # Ensure the tuple has at least 2 elements.
            if tuple_element_types.length < 2:
                raise SemanticErrors.INVALID_BINARY_FOLD_EXPR_ELEMENT_COUNT(self.rhs, tuple_element_types.length)

            # The LHS and RHS are altered for type-checking (elements of the tuple).
            self.lhs = Parser(Lexer(f"{self.rhs}.0").lex(), "").parse_expression(scope_handler).parse_once()
            self.rhs = Parser(Lexer(f"{self.rhs}.1").lex(), "").parse_expression(scope_handler).parse_once()

        if is_binary_foldr:
            self.lhs, self.rhs = self.rhs, self.lhs

        # Convert the binary expression to a function call. "1 + 2" becomes "1.add(2)".
        ast = BinaryExpressionAstUtils.fix_associativity(self)
        ast = BinaryExpressionAstUtils.combine_comparison_operators(ast)
        ast = BinaryExpressionAstUtils.convert_all_to_function(ast)
        self._as_func = ast
        self._as_func.do_semantic_analysis(scope_handler, **kwargs)
        return self._as_func

    def infer_type(self, scope_handler, **kwargs) -> InferredType:
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
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst

        # A Python-borrowed feature is the combination of comparison operators, such as "a < b < c". This function
        # recursively combines comparison operators into a single binary expression, so that "a < b < c" becomes
        # "a < b and b < c".

        def is_comparison_operator(token):
            return token in {TokenType.TkEq, TokenType.TkNe, TokenType.TkLt, TokenType.TkGt, TokenType.TkLe, TokenType.TkGe}

        # If the ast isn't a binary expression, or the binary operator is not comparable-combinable, then return the
        # current AST, whatever it is.
        if not isinstance(ast.lhs, BinaryExpressionAst) or not is_comparison_operator(ast.op.token.token_type):
            return ast

        # Combine the comparison into separate binary expressions, so that "a < b < c" becomes "a < b and b < c".
        lhs = ast.lhs
        rhs = ast.rhs
        lhs = lhs.rhs
        ast.rhs = BinaryExpressionAst(ast.pos, lhs, ast.op, rhs)
        ast.op  = TokenAst.dummy(TokenType.KwAnd)
        BinaryExpressionAstUtils.combine_comparison_operators(ast.lhs)

        return ast

    @staticmethod
    def convert_to_function(ast: BinaryExpressionAst) -> "PostfixExpressionAst":
        from SPPCompiler.SyntacticAnalysis.Parser import Parser
        from SPPCompiler.LexicalAnalysis.Lexer import Lexer

        # Transform the binary expression to a function call.
        func_name = BIN_OP_FUNCS[ast.op.token.token_type]
        code = f"{ast.lhs}.{func_name}({ast.rhs})"
        mock_function_call = Parser(Lexer(code).lex(), "", pos_shift=ast.pos).parse_expression().parse_once()
        return mock_function_call

    @staticmethod
    def convert_all_to_function(ast: BinaryExpressionAst) -> "PostfixExpressionAst":
        if not isinstance(ast, BinaryExpressionAst):
            return ast

        ast.lhs = BinaryExpressionAstUtils.convert_all_to_function(ast.lhs)
        ast.rhs = BinaryExpressionAstUtils.convert_all_to_function(ast.rhs)
        ast = BinaryExpressionAstUtils.convert_to_function(ast)

        # print(ast)
        return ast


__all__ = ["BinaryExpressionAst"]

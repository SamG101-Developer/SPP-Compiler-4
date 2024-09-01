from dataclasses import dataclass, field
from typing import Optional

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstOperators import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType, ensure_memory_integrity
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors


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

        # Compound assignment expressions must have a variable on the left-hand side.
        if self.op.token.token_type in {TokenType.TkAddAssign, TokenType.TkSubAssign, TokenType.TkMulAssign, TokenType.TkDivAssign, TokenType.TkModAssign, TokenType.TkRemAssign, TokenType.TkExpAssign}:
            lhs_symbol = scope_handler.current_scope.get_outermost_variable_symbol(self.lhs)
            if not lhs_symbol:
                raise SemanticErrors.INVALID_OPERAND_COMPOUND_ASSIGNMENT(self.op, self.lhs)

        if isinstance(self.lhs, TokenAst):
            tuple_element_count = len(self.rhs.infer_type(scope_handler, **kwargs).type.types[-1].generic_arguments.arguments)
            ensure_memory_integrity(self, self.rhs, self.op, scope_handler, mark_symbols=False)

            # Form an expanded AST for the tuple elements.
            unfolded_parts = []
            for i in range(tuple_element_count):
                tuple_access = f"{self.rhs}.{i}"
                unfolded_parts.append(Parser(Lexer(tuple_access).lex(), "", pos_shift=self.pos).parse_expression().parse_once())

            # Form the new binary tree with the unfolded parts.
            self.lhs, self.rhs = unfolded_parts[1], unfolded_parts[0]
            for part in unfolded_parts[2:]:
                self.lhs, self.rhs = part, BinaryExpressionAst(self.pos, self.lhs, self.op, self.rhs)

        elif isinstance(self.rhs, TokenAst):
            tuple_element_count = len(self.lhs.infer_type(scope_handler, **kwargs).type.types[-1].generic_arguments.arguments)
            ensure_memory_integrity(self, self.lhs, self.op, scope_handler, mark_symbols=False)

            # Form an expanded AST for the tuple elements.
            unfolded_parts = []
            for i in range(tuple_element_count):
                tuple_access = f"{self.lhs}.{i}"
                unfolded_parts.append(Parser(Lexer(tuple_access).lex(), "", pos_shift=self.pos).parse_expression().parse_once())

            # Form the new binary tree with the unfolded parts.
            self.lhs, self.rhs = unfolded_parts[0], unfolded_parts[1]
            for part in unfolded_parts[2:]:
                self.lhs, self.rhs = BinaryExpressionAst(self.pos, self.lhs, self.op, self.rhs), part

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
        
        # Todo: Handle "??" and "is" operators => no function mapping.

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

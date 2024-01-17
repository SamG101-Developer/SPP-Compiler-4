from __future__ import annotations

import functools
from typing import Callable, Tuple
from warnings import warn

from src.LexicalAnalysis.Tokens import Token, TokenType
from src.SyntacticAnalysis.ParserRuleHandler import ParserRuleHandler
from src.SyntacticAnalysis.ParserError import ParserError
from src.SemanticAnalysis.ASTs.Ast import *

from src.Utils.ErrorFormatter import ErrorFormatter


# Decorator that wraps the function in a ParserRuleHandler
def parser_rule(func):
    @functools.wraps(func)
    def wrapper(self=None, *args):
        return ParserRuleHandler(self, functools.partial(func, self, *args))
    return wrapper


def tested_parser_rule(func):
    func.__tested__ = True
    return func


def failed_parser_rule(func):
    func.__tested__ = False
    return func


class Parser:
    _tokens: List[Token]
    _index: int
    _err_fmt: ErrorFormatter
    _errors: List[ParserError]
    _pos_shift: int

    def __init__(self, tokens: List[Token], file_name: str, pos_shift: int = 0) -> None:
        self._tokens = tokens
        self._index = 0
        self._err_fmt = ErrorFormatter(self._tokens, file_name)
        self._errors = []
        self._pos_shift = pos_shift

    def current_pos(self) -> int:
        return self._index + self._pos_shift

    def current_tok(self) -> Token:
        return self._tokens[self._index]

    # Modify all function calls
    # def __getattribute__(self, item):
    #     attr = super().__getattribute__(item)
    #     if callable(attr) and item.startswith("parse_"):
    #         if not hasattr(attr, "__tested__"):
    #             print(f"[+] Parser function '{item}' has not been tested yet.")
    #         if hasattr(attr, "__tested__") and not attr.__tested__:
    #             print(f"[!] Parser function '{item}' is not working correctly.")
    #     return attr

    # ===== PARSING =====

    def parse(self) -> ProgramAst:
        try:
            return self.parse_program().parse_once()

        except ParserError as e:
            final_error = self._errors[0]

            for current_error in self._errors:
                if current_error.pos > final_error.pos:
                    final_error = current_error

            all_expected_tokens = "['" + "' | '".join(final_error.expected_tokens).replace("\n", "\\n") + "']"
            error_message = str(final_error).replace("$", all_expected_tokens)
            error_message = self._err_fmt.error(final_error.pos, message=error_message)
            raise SystemExit(error_message) from None

    # ===== PROGRAM =====

    @parser_rule
    @tested_parser_rule
    def parse_program(self) -> ProgramAst:
        c1 = self.current_pos()
        p1 = self.parse_module_prototype().parse_once()
        p2 = self.parse_eof().parse_once()
        return ProgramAst(c1, p1, p2)

    @parser_rule
    @tested_parser_rule
    def parse_eof(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkEOF).parse_once()
        return p1

    # ===== MODULES =====

    @parser_rule
    @tested_parser_rule
    def parse_module_prototype(self) -> ModulePrototypeAst:
        """
        [ModulePrototype] => [Annotation]* [Tok("mod")] [ModuleIdentifier] [ModuleMember]*

        - [Annotation]*      => The list of token stream mutations to make to the module before it is parsed.
        - [Tok("mod")]       => The 'mod' keyword.
        - [ModuleIdentifier] => The identifier of the module.
        - [ModuleMember]*    => The list of members that form the implementation of the module.

        The ModulePrototype contains the members that form a single module. It is a "top-level" AST, because the
        ProgramAst stores a ModulePrototypeAst instance directly.
        """

        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_token(TokenType.KwMod).parse_once()
        p3 = self.parse_module_identifier().parse_once()
        p4 = self.parse_module_implementation().parse_once()
        return ModulePrototypeAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_module_implementation(self) -> ModuleImplementationAst:
        c1 = self.current_pos()
        p1 = self.parse_module_member().parse_zero_or_more()
        return ModuleImplementationAst(c1, p1)

    @parser_rule
    @tested_parser_rule
    def parse_module_member(self) -> ModuleMemberAst:
        p1 = self.parse_function_prototype().for_alt()
        p2 = self.parse_class_prototype().for_alt()
        p3 = self.parse_sup_prototype_inheritance().for_alt()
        p4 = self.parse_sup_prototype_normal().for_alt()
        p5 = self.parse_typedef_statement().for_alt()
        p6 = (p1 | p2 | p3 | p4 | p5).parse_once()
        return p6

    @parser_rule
    @tested_parser_rule
    def parse_module_identifier(self) -> ModuleIdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_one_or_more(TokenType.TkDot)
        return ModuleIdentifierAst(c1, p1)

    # ===== CLASSES =====

    @parser_rule
    @tested_parser_rule
    def parse_class_prototype(self) -> ClassPrototypeAst:
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_token(TokenType.KwCls).parse_once()
        p3 = self.parse_upper_identifier().parse_once()
        p4 = self.parse_generic_parameters().parse_optional()
        p5 = self.parse_where_block().parse_optional()
        p6 = self.parse_inner_scope(self.parse_class_attribute).parse_once()
        return ClassPrototypeAst(c1, p1, p2, TypeSingleAst(p3.pos, [GenericIdentifierAst(p3.pos, p3.value, None)]), p4, p5, p6)

    @parser_rule
    @tested_parser_rule
    def parse_class_attribute(self) -> ClassAttributeAst:
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_identifier().parse_once()
        p3 = self.parse_token(TokenType.TkColon).parse_once()
        p4 = self.parse_type().parse_once()
        return ClassAttributeAst(c1, p1, p2, p3, p4)

    # ===== SUPERIMPOSITION =====

    @parser_rule
    @tested_parser_rule
    def parse_sup_prototype_normal(self) -> SupPrototypeNormalAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwSup).parse_once()
        p2 = self.parse_generic_parameters().parse_optional()
        p3 = self.parse_type().parse_once()
        p4 = self.parse_where_block().parse_optional()
        p5 = self.parse_inner_scope(self.parse_sup_member).parse_once()
        return SupPrototypeNormalAst(c1, p1, p2, p3, p4, p5)

    @parser_rule
    @tested_parser_rule
    def parse_sup_prototype_inheritance(self) -> SupPrototypeInheritanceAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwSup).parse_once()
        p2 = self.parse_generic_parameters().parse_optional()
        p3 = self.parse_type().parse_once()
        p4 = self.parse_token(TokenType.KwOn).parse_once()
        p5 = self.parse_type().parse_once()
        p6 = self.parse_where_block().parse_optional()
        p7 = self.parse_inner_scope(self.parse_sup_member).parse_once()
        return SupPrototypeInheritanceAst(c1, p1, p2, p5, p6, p7, p3, p4)

    @parser_rule
    @tested_parser_rule
    def parse_sup_member(self) -> SupMemberAst:
        p1 = self.parse_sup_method_prototype().for_alt()
        p2 = self.parse_sup_typedef().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_sup_typedef(self) -> SupTypedefAst:
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_typedef_statement().parse_once()
        return SupTypedefAst(**p2.__dict__, annotations=p1)

    @parser_rule
    @tested_parser_rule
    def parse_sup_method_prototype(self) -> SupMethodPrototypeAst:
        p1 = self.parse_function_prototype().parse_once()
        return SupMethodPrototypeAst(**p1.__dict__)

    # ===== FUNCTIONS =====

    @parser_rule
    @tested_parser_rule
    def parse_function_prototype(self) -> FunctionPrototypeAst:
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_token(TokenType.KwFun).parse_once()
        p3 = self.parse_identifier().parse_once()
        p4 = self.parse_generic_parameters().parse_optional()
        p5 = self.parse_function_parameters().parse_once()
        p6 = self.parse_token(TokenType.TkArrowR).parse_once()
        p7 = self.parse_type().parse_once()
        p8 = self.parse_where_block().parse_optional()
        p9 = self.parse_inner_scope(self.parse_statement).parse_once()
        return FunctionPrototypeAst(c1, p1, p2, p3, p4, p5, p6, p7, p8, p9)

    @parser_rule
    @tested_parser_rule
    def parse_function_call_arguments(self) -> FunctionArgumentGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_function_call_argument().parse_zero_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return FunctionArgumentGroupAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_function_call_argument(self) -> FunctionArgumentAst:
        p1 = self.parse_function_call_argument_named().for_alt()
        p2 = self.parse_function_call_argument_normal().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_function_call_argument_normal(self) -> FunctionArgumentNormalAst:
        c1 = self.current_pos()
        p1 = self.parse_convention().parse_once()
        p2 = self.parse_token(TokenType.TkVariadic).parse_optional()
        p3 = self.parse_expression().parse_once()
        return FunctionArgumentNormalAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_function_call_argument_named(self) -> FunctionArgumentNamedAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_convention().parse_once()
        p4 = self.parse_expression().parse_once()
        return FunctionArgumentNamedAst(c1, p1, p2, p3, p4)

    @parser_rule
    @tested_parser_rule
    def parse_function_parameters(self) -> FunctionParameterGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_function_parameter().parse_zero_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return FunctionParameterGroupAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_function_parameter(self) -> FunctionParameterAst:
        p1 = self.parse_function_parameter_variadic().for_alt()
        p2 = self.parse_function_parameter_optional().for_alt()
        p3 = self.parse_function_parameter_required().for_alt()
        p4 = self.parse_function_parameter_self().for_alt()
        p5 = (p1 | p2 | p3 | p4).parse_once()
        return p5

    @parser_rule
    @tested_parser_rule
    def parse_function_parameter_self(self) -> FunctionParameterSelfAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwMut).parse_optional()
        p2 = self.parse_convention().parse_once()
        p3 = self.parse_self_keyword().parse_once()
        return FunctionParameterSelfAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_function_parameter_required(self) -> FunctionParameterRequiredAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwMut).parse_optional()
        p2 = self.parse_identifier().parse_once()
        p3 = self.parse_token(TokenType.TkColon).parse_once()
        p4 = self.parse_convention().parse_once()
        p5 = self.parse_type().parse_once()
        return FunctionParameterRequiredAst(c1, p1, p2, p3, p4, p5)

    @parser_rule
    @tested_parser_rule
    def parse_function_parameter_optional(self) -> FunctionParameterOptionalAst:
        p1 = self.parse_function_parameter_required().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_expression().parse_once()
        return FunctionParameterOptionalAst(**p1.__dict__, assignment_token=p2, default_value=p3)

    @parser_rule
    @tested_parser_rule
    def parse_function_parameter_variadic(self) -> FunctionParameterVariadicAst:
        p1 = self.parse_token(TokenType.TkVariadic).parse_once()
        p2 = self.parse_function_parameter_required().parse_once()
        return FunctionParameterVariadicAst(**p2.__dict__, variadic_token=p1)

    # ===== GENERICS =====

    @parser_rule
    @tested_parser_rule
    def parse_generic_arguments(self) -> GenericArgumentGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBrackL).parse_once()
        p2 = self.parse_generic_argument().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBrackR).parse_once()
        return GenericArgumentGroupAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_generic_argument(self) -> GenericArgumentAst:
        p1 = self.parse_generic_argument_named().for_alt()
        p2 = self.parse_generic_argument_normal().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_generic_argument_normal(self) -> GenericArgumentNormalAst:
        c1 = self.current_pos()
        p1 = self.parse_type().parse_once()
        return GenericArgumentNormalAst(c1, p1)

    @parser_rule
    @tested_parser_rule
    def parse_generic_argument_named(self) -> GenericArgumentNamedAst:
        c1 = self.current_pos()
        p1 = self.parse_upper_identifier().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_generic_argument_normal().parse_once()
        return GenericArgumentNamedAst(**p3.__dict__, identifier=p1, assignment_token=p2)

    @parser_rule
    @tested_parser_rule
    def parse_generic_parameters(self) -> GenericParameterGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBrackL).parse_once()
        p2 = self.parse_generic_parameter().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBrackR).parse_once()
        return GenericParameterGroupAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_generic_parameter(self) -> GenericParameterAst:
        p1 = self.parse_generic_parameter_variadic().for_alt()
        p2 = self.parse_generic_parameter_optional().for_alt()
        p3 = self.parse_generic_parameter_required().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    @tested_parser_rule
    def parse_generic_parameter_required(self) -> GenericParameterRequiredAst:
        c1 = self.current_pos()
        p1 = self.parse_upper_identifier().parse_once()
        p2 = self.parse_generic_inline_constraints().parse_optional()
        return GenericParameterRequiredAst(c1, p1, p2)

    @parser_rule
    @tested_parser_rule
    def parse_generic_parameter_optional(self) -> GenericParameterOptionalAst:
        p1 = self.parse_generic_parameter_required().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_type().parse_once()
        return GenericParameterOptionalAst(**p1.__dict__, assignment_token=p2, default_value=p3)

    @parser_rule
    @tested_parser_rule
    def parse_generic_parameter_variadic(self) -> GenericParameterVariadicAst:
        p1 = self.parse_token(TokenType.TkVariadic).parse_once()
        p2 = self.parse_generic_parameter_required().parse_once()
        return GenericParameterVariadicAst(**p2.__dict__, variadic_token=p1)

    @parser_rule
    @tested_parser_rule
    def parse_generic_inline_constraints(self) -> GenericParameterInlineConstraintAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkColon).parse_once()
        p2 = self.parse_type().parse_one_or_more(TokenType.TkBitAnd)
        return GenericParameterInlineConstraintAst(c1, p1, p2)

    # ===== WHERE =====

    @parser_rule
    @tested_parser_rule
    def parse_where_block(self) -> WhereBlockAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwWhere).parse_once()
        p2 = self.parse_where_block_constraints_group().parse_once()
        return WhereBlockAst(c1, p1, p2)

    @parser_rule
    @tested_parser_rule
    def parse_where_block_constraints_group(self) -> WhereConstraintsGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBrackL).parse_once()
        p2 = self.parse_where_block_constraints().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBrackR).parse_once()
        return WhereConstraintsGroupAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_where_block_constraints(self) -> WhereConstraintsAst:
        c1 = self.current_pos()
        p1 = self.parse_type().parse_one_or_more(TokenType.TkComma)
        p2 = self.parse_token(TokenType.TkColon).parse_once()
        p3 = self.parse_type().parse_one_or_more(TokenType.TkBitAnd)
        return WhereConstraintsAst(c1, p1, p2, p3)

    # ===== ANNOTATIONS =====

    @parser_rule
    @tested_parser_rule
    def parse_annotation(self) -> AnnotationAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkAt).parse_once()
        return None
        # raise NotImplementedError("Annotations won't be supported until compiler is self-hosting.")

    # ===== EXPRESSIONS =====

    @parser_rule
    @tested_parser_rule
    def parse_expression(self) -> ExpressionAst:
        p1 = self.parse_binary_expression_precedence_level_1().parse_once()
        return p1

    @parser_rule
    @tested_parser_rule
    def parse_binary_expression_precedence_level_n_rhs(self, op, rhs) -> Tuple[TokenAst, ExpressionAst]:
        p1 = op().parse_once()
        p2 = rhs().parse_once()
        return p1, p2

    @parser_rule
    @tested_parser_rule
    def parse_binary_expression_precedence_level_n(self, lhs, op, rhs) -> BinaryExpressionAst:
        c1 = self.current_pos()
        p1 = lhs().parse_once()
        p2 = self.parse_binary_expression_precedence_level_n_rhs(op, rhs).parse_optional()
        return BinaryExpressionAst(c1, p1, p2[0], p2[1]) if p2 else p1

    @tested_parser_rule
    def parse_binary_expression_precedence_level_1(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_2,
            self.parse_binary_op_precedence_level_1,
            self.parse_binary_expression_precedence_level_1)

    @tested_parser_rule
    def parse_binary_expression_precedence_level_2(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_3,
            self.parse_binary_op_precedence_level_2,
            self.parse_binary_expression_precedence_level_2)

    @tested_parser_rule
    def parse_binary_expression_precedence_level_3(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_4,
            self.parse_binary_op_precedence_level_3,
            self.parse_binary_expression_precedence_level_3)

    @tested_parser_rule
    def parse_binary_expression_precedence_level_4(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_5,
            self.parse_binary_op_precedence_level_4,
            self.parse_binary_expression_precedence_level_4)

    @tested_parser_rule
    def parse_binary_expression_precedence_level_5(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_6,
            self.parse_binary_op_precedence_level_5,
            self.parse_binary_expression_precedence_level_5)

    @tested_parser_rule
    def parse_binary_expression_precedence_level_6(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_7,
            self.parse_binary_op_precedence_level_6,
            self.parse_binary_expression_precedence_level_6)

    @tested_parser_rule
    def parse_binary_expression_precedence_level_7(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_unary_expression,
            self.parse_binary_op_precedence_level_7,
            self.parse_binary_expression_precedence_level_7)

    @parser_rule
    @tested_parser_rule
    def parse_unary_expression(self) -> ExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_unary_op().parse_zero_or_more()
        p2 = self.parse_postfix_expression().parse_once()
        return functools.reduce(lambda acc, x: UnaryExpressionAst(c1, x, acc), p1, p2)

    @parser_rule
    @tested_parser_rule
    def parse_postfix_expression(self) -> ExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_primary_expression().parse_once()
        p2 = self.parse_postfix_op().parse_zero_or_more()
        return functools.reduce(lambda acc, x: PostfixExpressionAst(c1, acc, x), p2, p1)

    @parser_rule
    @tested_parser_rule
    def parse_primary_expression(self) -> ExpressionAst:
        p1 = self.parse_literal().for_alt()
        p2 = self.parse_object_initialization().for_alt()
        p3 = self.parse_lambda_prototype().for_alt()
        p4 = self.parse_parenthesized_expression().for_alt()
        p5 = self.parse_identifier().for_alt()
        p6 = self.parse_if_expression().for_alt()
        p7 = self.parse_while_expression().for_alt()
        p8 = self.parse_yield_expression().for_alt()
        p9 = self.parse_with_expression().for_alt()
        p10 = self.parse_inner_scope(self.parse_statement).for_alt()
        p11 = self.parse_self_keyword().for_alt()
        p12 = self.parse_token(TokenType.TkVariadic).for_alt()
        p13 = (p1 | p2 | p3 | p4 | p5 | p6 | p7 | p8 | p9 | p10 | p11 | p12).parse_once()
        return p13

    @parser_rule
    @tested_parser_rule
    def parse_parenthesized_expression(self) -> ParenthesizedExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_expression().parse_once()
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return ParenthesizedExpressionAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_self_keyword(self) -> IdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwSelf).parse_once()
        return IdentifierAst(c1, p1.token.token_metadata)

    # ===== EXPRESSION STATEMENTS =====

    @parser_rule
    @tested_parser_rule
    def parse_if_expression(self) -> IfExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwIf).parse_once()
        p2 = self.parse_expression().parse_once()
        p3 = self.parse_pattern_comp_op().parse_optional()
        # p4 = self.parse_pattern_statement().parse_one_or_more(TokenType.TkNewLine)
        p4 = self.parse_inner_scope(self.parse_pattern_statement).parse_once()
        return IfExpressionAst(c1, p1, p2, p3, p4)

    @parser_rule
    @tested_parser_rule
    def parse_while_expression(self) -> WhileExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwWhile).parse_once()
        p2 = self.parse_expression().parse_once()
        p3 = self.parse_inner_scope(self.parse_statement).parse_once()
        p4 = self.parse_residual_inner_scope().parse_optional()
        return WhileExpressionAst(c1, p1, p2, p3, p4)

    @parser_rule
    @tested_parser_rule
    def parse_yield_expression(self) -> YieldExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwYield).parse_once()
        p2 = self.parse_token(TokenType.KwWith).parse_optional()
        p3 = self.parse_convention().parse_once()
        p4 = self.parse_expression().parse_optional()
        return YieldExpressionAst(c1, p1, p2, p3, p4)

    @parser_rule
    @tested_parser_rule
    def parse_with_expression(self) -> WithExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwWith).parse_once()
        p2 = self.parse_with_expression_lhs_alias().parse_optional()
        p3 = self.parse_expression().parse_once()
        p4 = self.parse_inner_scope(self.parse_statement).parse_once()
        return WithExpressionAst(c1, p1, p2, p3, p4)

    @parser_rule
    @tested_parser_rule
    def parse_with_expression_lhs_alias(self) -> WithExpressionAliasAst:
        c1 = self.current_pos()
        p1 = self.parse_local_variable().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        return WithExpressionAliasAst(c1, p1, p2)

    # ===== STATEMENTS =====

    @parser_rule
    @tested_parser_rule
    def parse_return_statement(self) -> ReturnStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwRet).parse_once()
        p2 = self.parse_expression().parse_optional()
        return ReturnStatementAst(c1, p1, p2)

    @parser_rule
    @tested_parser_rule
    def parse_inner_scope(self, rule) -> InnerScopeAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBraceL).parse_once()
        p2 = rule().parse_zero_or_more(TokenType.TkNewLine)
        p3 = self.parse_token(TokenType.TkBraceR).parse_once()
        return InnerScopeAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_residual_inner_scope(self) -> ResidualInnerScopeAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwElse).parse_once()
        p2 = self.parse_inner_scope(self.parse_statement).parse_once()
        return ResidualInnerScopeAst(c1, p1, p2)

    @parser_rule
    @tested_parser_rule
    def parse_statement(self):
        p1 = self.parse_typedef_statement().for_alt()
        p2 = self.parse_let_statement().for_alt()
        p3 = self.parse_return_statement().for_alt()
        p4 = self.parse_assignment_statement().for_alt()
        p5 = self.parse_expression().for_alt()
        p6 = (p1 | p2 | p3 | p4 | p5).parse_once()
        return p6

    # ===== TYPEDEFS =====

    @parser_rule
    @tested_parser_rule
    def parse_typedef_statement(self) -> TypedefStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwUse).parse_once()
        p2 = self.parse_generic_parameters().parse_optional()
        p3 = self.parse_type_namespace().parse_optional()
        p4 = self.parse_typedef_item().parse_once()
        return TypedefStatementAst(c1, p1, p2, p3, p4)

    @parser_rule
    @tested_parser_rule
    def parse_typedef_item(self) -> TypedefStatementItemAst:
        p1 = self.parse_typedef_statement_specific_item().for_alt()
        p2 = self.parse_typedef_statement_specific_items().for_alt()
        p3 = self.parse_typedef_statement_all_items().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_typedef_statement_specific_item(self) -> TypedefStatementSpecificItemAst:
        c1 = self.current_pos()
        p1 = self.parse_type_single().parse_once()
        p2 = self.parse_typedef_statement_specific_item_alias().parse_optional()
        return TypedefStatementSpecificItemAst(c1, p1, p2)

    @parser_rule
    @tested_parser_rule
    def parse_typedef_statement_specific_items(self) -> TypedefStatementSpecificItemsAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBraceL).parse_once()
        p2 = self.parse_typedef_statement_specific_item().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBraceR).parse_once()
        return TypedefStatementSpecificItemsAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_typedef_statement_all_items(self) -> TypedefStatementAllItemsAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkMul).parse_once()
        return TypedefStatementAllItemsAst(c1, p1)

    @parser_rule
    @tested_parser_rule
    def parse_typedef_statement_specific_item_alias(self) -> TypedefStatementSpecificItemAliasAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwAs).parse_once()
        p2 = self.parse_generic_identifier().parse_once()
        return TypedefStatementSpecificItemAliasAst(c1, p1, p2)

    # ===== LET-DECLARATIONS =====

    @parser_rule
    @tested_parser_rule
    def parse_let_statement(self) -> LetStatementAst:
        p1 = self.parse_let_statement_initialized().for_alt()
        p2 = self.parse_let_statement_uninitialized().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_let_statement_initialized(self) -> LetStatementInitializedAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwLet).parse_once()
        p2 = self.parse_local_variable().parse_once()
        p3 = self.parse_token(TokenType.TkAssign).parse_once()
        p4 = self.parse_expression().parse_once()
        p5 = self.parse_residual_inner_scope().parse_optional()
        return LetStatementInitializedAst(c1, p1, p2, p3, p4, p5)

    @parser_rule
    @tested_parser_rule
    def parse_let_statement_uninitialized(self) -> LetStatementUninitializedAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwLet).parse_once()
        p2 = self.parse_local_variable().parse_once()
        p3 = self.parse_token(TokenType.TkColon).parse_once()
        p4 = self.parse_type().parse_once()
        return LetStatementUninitializedAst(c1, p1, p2, p3, p4)

    @parser_rule
    @tested_parser_rule
    def parse_local_variable(self) -> LocalVariableAst:
        p1 = self.parse_local_variable_single().for_alt()
        p2 = self.parse_local_variable_tuple().for_alt()
        p3 = self.parse_local_variable_destructure().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    @tested_parser_rule
    def parse_local_variable_single(self) -> LocalVariableSingleAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwMut).parse_optional()
        p2 = self.parse_token(TokenType.TkVariadic).parse_optional()
        p3 = self.parse_identifier().parse_once()
        return LocalVariableSingleAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_local_variable_tuple(self) -> LocalVariableTupleAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_local_variable().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return LocalVariableTupleAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_local_variable_destructure(self) -> LocalVariableDestructureAst:
        # TODO : allow for ".." to represent "the rest of the tuple"
        # TODO : allow this ".." to be unnamed (adjust parse_local_variable_single to allow for unnamed variables)
        # TODO : use semantic analysis to only allow unnamed variables in certain places

        c1 = self.current_pos()
        p1 = self.parse_type_single().parse_once()
        p2 = self.parse_token(TokenType.TkBraceL).parse_once()
        p3 = self.parse_local_variable().parse_one_or_more(TokenType.TkComma)
        p4 = self.parse_token(TokenType.TkBraceR).parse_once()
        return LocalVariableDestructureAst(c1, p1, p2, p3, p4)

    # ===== ASSIGNMENT =====

    @parser_rule
    @tested_parser_rule
    def parse_assignment_statement(self) -> AssignmentStatementAst:
        p1 = self.parse_assignment_statement_single().for_alt()
        p2 = self.parse_assignment_statement_multi().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_assignment_statement_single(self) -> AssignmentStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_expression().parse_once()
        p2 = self.parse_assignment_op().parse_once()
        p3 = self.parse_expression().parse_once()
        return AssignmentStatementAst(c1, [p1], p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_assignment_statement_multi(self) -> AssignmentStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_expression().parse_one_or_more(TokenType.TkComma)
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_expression().parse_once()
        return AssignmentStatementAst(c1, p1, p2, p3)

    # ===== PATTERNS =====

    @parser_rule
    def parse_pattern_statement(self) -> PatternBlockAst:
        c1 = self.current_pos()
        p1 = self.parse_pattern_comp_op().parse_optional()
        p2 = self.parse_pattern_variant().parse_one_or_more(TokenType.TkBitOr)
        p3 = self.parse_pattern_guard().parse_optional()
        p4 = self.parse_inner_scope(self.parse_statement).parse_once()
        return PatternBlockAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_pattern_comp_op(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkEq).for_alt()
        p2 = self.parse_token(TokenType.TkNe).for_alt()
        p3 = self.parse_token(TokenType.TkLe).for_alt()
        p4 = self.parse_token(TokenType.TkGe).for_alt()
        p5 = self.parse_token(TokenType.TkLt).for_alt()
        p6 = self.parse_token(TokenType.TkGt).for_alt()
        p7 = self.parse_token(TokenType.KwIs).for_alt()
        p8 = (p1 | p2 | p3 | p4 | p5 | p6 | p7).parse_once()
        return p8

    @parser_rule
    def parse_pattern_guard(self) -> PatternGuardAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkLogicalAnd).parse_once()
        p2 = self.parse_expression().parse_once()
        return PatternGuardAst(c1, p1, p2)

    @parser_rule
    @tested_parser_rule
    def parse_pattern_variant(self) -> PatternVariantAst:
        p1 = self.parse_pattern_variant_tuple().for_alt()
        p2 = self.parse_pattern_variant_destructure().for_alt()
        p3 = self.parse_pattern_variant_variable().for_alt()
        p4 = self.parse_pattern_variant_literal().for_alt()
        # p5 = self.parse_pattern_variant_bool_member().for_alt()
        p6 = self.parse_pattern_variant_else().for_alt()
        p7 = (p1 | p2 | p3 | p4 | p6).parse_once()
        return p7

    # TODO : change these pattern variants:
    #   1. Copy parser code out of function call in these parsers
    #   2. Change the "local-variable" part to a "pattern" part

    @parser_rule
    @tested_parser_rule
    def parse_pattern_variant_tuple(self) -> PatternVariantTupleAst:
        c1 = self.current_pos()
        p1 = self.parse_local_variable_tuple().parse_once()
        return PatternVariantTupleAst(c1, p1)

    @parser_rule
    @failed_parser_rule
    def parse_pattern_variant_destructure(self) -> PatternVariantDestructureAst:
        c1 = self.current_pos()
        p1 = self.parse_local_variable_destructure().parse_once()
        return PatternVariantDestructureAst(c1, p1)

    @parser_rule
    @tested_parser_rule
    def parse_pattern_variant_variable(self) -> PatternVariantVariableAst:
        c1 = self.current_pos()
        p1 = self.parse_local_variable_single().parse_once()
        return PatternVariantVariableAst(c1, p1)

    @parser_rule
    @tested_parser_rule
    def parse_pattern_variant_literal(self) -> PatternVariantLiteralAst:
        c1 = self.current_pos()
        p1 = self.parse_literal().parse_once()
        return PatternVariantLiteralAst(c1, p1)

    @parser_rule
    @failed_parser_rule
    def parse_pattern_variant_bool_member(self) -> PatternVariantBoolMemberAst:
        c1 = self.current_pos()
        p1 = self.parse_postfix_op().parse_zero_or_more()
        return PatternVariantBoolMemberAst(c1, functools.reduce(lambda acc, x: PostfixExpressionAst(c1, acc, x), p1, None))

    @parser_rule
    @tested_parser_rule
    def parse_pattern_variant_else(self) -> PatternVariantElseAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwElse).parse_once()
        return PatternVariantElseAst(c1, p1)

    # ===== OPERATORS =====

    @parser_rule
    @tested_parser_rule
    def parse_binary_op_precedence_level_1(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkCoalesce).parse_once()
        return p1

    @parser_rule
    @tested_parser_rule
    def parse_binary_op_precedence_level_2(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkLogicalOr).parse_once()
        return p1

    @parser_rule
    @tested_parser_rule
    def parse_binary_op_precedence_level_3(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkLogicalAnd).parse_once()
        return p1

    @parser_rule
    @tested_parser_rule
    def parse_binary_op_precedence_level_4(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkEq).for_alt()
        p2 = self.parse_token(TokenType.TkNe).for_alt()
        p3 = self.parse_token(TokenType.TkLe).for_alt()
        p4 = self.parse_token(TokenType.TkGe).for_alt()
        p5 = self.parse_token(TokenType.TkLt).for_alt()
        p6 = self.parse_token(TokenType.TkGt).for_alt()
        p7 = self.parse_token(TokenType.TkSs).for_alt()
        p8 = self.parse_token(TokenType.KwIs).for_alt()
        p9 = (p1 | p2 | p3 | p4 | p5 | p6 | p7 | p8).parse_once()
        return p9

    @parser_rule
    @tested_parser_rule
    def parse_binary_op_precedence_level_5(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkBitShiftL).for_alt()
        p2 = self.parse_token(TokenType.TkBitShiftR).for_alt()
        p3 = self.parse_token(TokenType.TkBitRotateL).for_alt()
        p4 = self.parse_token(TokenType.TkBitRotateR).for_alt()
        p5 = (p1 | p2 | p3 | p4).parse_once()
        return p5

    @parser_rule
    @tested_parser_rule
    def parse_binary_op_precedence_level_6(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkBitOr).for_alt()
        p2 = self.parse_token(TokenType.TkBitXor).for_alt()
        p3 = self.parse_token(TokenType.TkAdd).for_alt()
        p4 = self.parse_token(TokenType.TkSub).for_alt()
        p5 = (p1 | p2 | p3 | p4).parse_once()
        return p5

    @parser_rule
    @tested_parser_rule
    def parse_binary_op_precedence_level_7(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkBitAnd).for_alt()
        p2 = self.parse_token(TokenType.TkMul).for_alt()
        p3 = self.parse_token(TokenType.TkDiv).for_alt()
        p4 = self.parse_token(TokenType.TkRem).for_alt()
        p5 = self.parse_token(TokenType.TkMod).for_alt()
        p6 = self.parse_token(TokenType.TkExp).for_alt()
        p7 = (p1 | p2 | p3 | p4 | p5 | p6).parse_once()
        return p7

    @parser_rule
    @tested_parser_rule
    def parse_unary_op(self) -> TokenAst:
        p1 = self.parse_token(TokenType.KwAsync).parse_once()
        return p1

    @parser_rule
    @tested_parser_rule
    def parse_postfix_op(self) -> TokenAst:
        p1 = self.parse_postfix_op_function_call().for_alt()
        p2 = self.parse_postfix_op_member_access().for_alt()
        p3 = self.parse_postfix_op_early_return().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    @tested_parser_rule
    def parse_postfix_op_function_call(self) -> PostfixExpressionOperatorFunctionCallAst:
        c1 = self.current_pos()
        p1 = self.parse_generic_arguments().parse_optional()
        p2 = self.parse_function_call_arguments().parse_once()
        p3 = self.parse_token(TokenType.TkVariadic).parse_optional()
        return PostfixExpressionOperatorFunctionCallAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_postfix_op_member_access(self) -> PostfixExpressionOperatorMemberAccessAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkDot).parse_once()
        p2 = self.parse_identifier().for_alt()
        p3 = self.parse_token(TokenType.LxDecDigits).for_alt()
        p4 = (p2 | p3).parse_once()
        return PostfixExpressionOperatorMemberAccessAst(c1, p1, p4)

    @parser_rule
    @tested_parser_rule
    def parse_postfix_op_early_return(self) -> PostfixExpressionOperatorEarlyReturnAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkQst).parse_once()
        return PostfixExpressionOperatorEarlyReturnAst(c1, p1)

    @parser_rule
    @tested_parser_rule
    def parse_assignment_op(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkLogicalOrAssign).for_alt()
        p2 = self.parse_token(TokenType.TkLogicalAndAssign).for_alt()
        p3 = self.parse_token(TokenType.TkBitOrAssign).for_alt()
        p4 = self.parse_token(TokenType.TkBitXorAssign).for_alt()
        p5 = self.parse_token(TokenType.TkBitAndAssign).for_alt()
        p6 = self.parse_token(TokenType.TkBitShiftLAssign).for_alt()
        p7 = self.parse_token(TokenType.TkBitShiftRAssign).for_alt()
        p8 = self.parse_token(TokenType.TkBitRotateLAssign).for_alt()
        p9 = self.parse_token(TokenType.TkBitRotateRAssign).for_alt()
        p10 = self.parse_token(TokenType.TkAddAssign).for_alt()
        p11 = self.parse_token(TokenType.TkSubAssign).for_alt()
        p12 = self.parse_token(TokenType.TkMulAssign).for_alt()
        p13 = self.parse_token(TokenType.TkDivAssign).for_alt()
        p14 = self.parse_token(TokenType.TkRemAssign).for_alt()
        p15 = self.parse_token(TokenType.TkModAssign).for_alt()
        p16 = self.parse_token(TokenType.TkExpAssign).for_alt()
        p17 = (p1 | p2 | p3 | p4 | p5 | p6 | p7 | p8 | p9 | p10 | p11 | p12 | p13 | p14 | p15 | p16).parse_once()
        return p17

    # ===== CONVENTIONS =====

    @parser_rule
    @tested_parser_rule
    def parse_convention(self) -> ConventionAst:
        p1 = self.parse_convention_mut().for_alt()
        p2 = self.parse_convention_ref().for_alt()
        p3 = self.parse_convention_mov().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    @tested_parser_rule
    def parse_convention_mov(self) -> ConventionMovAst:
        c1 = self.current_pos()
        return ConventionMovAst(c1)

    @parser_rule
    @tested_parser_rule
    def parse_convention_ref(self) -> ConventionRefAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBitAnd).parse_once()
        return ConventionRefAst(c1, p1)

    @parser_rule
    @tested_parser_rule
    def parse_convention_mut(self) -> ConventionMutAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBitAnd).parse_once()
        p2 = self.parse_token(TokenType.KwMut).parse_once()
        return ConventionMutAst(c1, p1, p2)

    # ===== OBJECT INITIALIZATION =====

    @parser_rule
    @tested_parser_rule
    def parse_object_initialization(self) -> ObjectInitializerAst:
        c1 = self.current_pos()
        p1 = self.parse_type_single().parse_once()
        p2 = self.parse_object_initializer_arguments().parse_once()
        return ObjectInitializerAst(c1, p1, p2)

    @parser_rule
    @tested_parser_rule
    def parse_object_initializer_arguments(self) -> ObjectInitializerArgumentGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBraceL).parse_once()
        p2 = self.parse_object_initializer_argument().parse_zero_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBraceR).parse_once()
        return ObjectInitializerArgumentGroupAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_object_initializer_argument(self) -> ObjectInitializerArgumentAst:
        p1 = self.parse_object_initializer_argument_named().for_alt()
        p2 = self.parse_object_initializer_argument_normal().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_object_initializer_argument_normal(self) -> ObjectInitializerArgumentNormalAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_once()
        return ObjectInitializerArgumentNormalAst(c1, p1)

    @parser_rule
    @tested_parser_rule
    def parse_object_initializer_argument_named(self) -> ObjectInitializerArgumentNamedAst:
        c1 = self.current_pos()
        p1 = self.parse_object_initializer_argument_named_key().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_expression().parse_once()
        return ObjectInitializerArgumentNamedAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_object_initializer_argument_named_key(self) -> IdentifierAst | TokenAst:
        p1 = self.parse_identifier().for_alt()
        p2 = self.parse_token(TokenType.KwElse).for_alt()
        p3 = self.parse_token(TokenType.KwSup).for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    # ===== LAMBDAS =====

    @parser_rule
    @tested_parser_rule
    def parse_lambda_prototype(self) -> LambdaPrototypeAst:
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_token(TokenType.KwFun).parse_once()
        p3 = self.parse_generic_parameters().parse_optional()
        p4 = self.parse_function_parameters().parse_once()
        p5 = self.parse_token(TokenType.TkArrowR).parse_once()
        p6 = self.parse_type().parse_once()
        p7 = self.parse_lambda_capture_block().parse_optional()
        p8 = self.parse_where_block().parse_optional()
        p9 = self.parse_inner_scope(self.parse_statement).parse_once()
        return LambdaPrototypeAst(c1, p1, p2, p3, p4, p5, p6, p7, p8, p9)

    @parser_rule
    @tested_parser_rule
    def parse_lambda_capture_block(self) -> LambdaCaptureBlockAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwWith).parse_once()
        p2 = self.parse_token(TokenType.TkBrackL).parse_once()
        p3 = self.parse_lambda_capture_item().parse_one_or_more(TokenType.TkComma)
        p4 = self.parse_token(TokenType.TkBrackR).parse_once()
        return LambdaCaptureBlockAst(c1, p1, p2, p3, p4)

    @parser_rule
    @tested_parser_rule
    def parse_lambda_capture_item(self) -> LambdaCaptureItemAst:
        p1 = self.parse_lambda_capture_item_named().for_alt()
        p2 = self.parse_lambda_capture_item_normal().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_lambda_capture_item_normal(self):
        c1 = self.current_pos()
        p1 = self.parse_convention().parse_once()
        p2 = self.parse_expression().parse_once()
        return LambdaCaptureItemNormalAst(c1, p1, p2)

    @parser_rule
    @tested_parser_rule
    def parse_lambda_capture_item_named(self):
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_convention().parse_once()
        p4 = self.parse_expression().parse_once()
        return LambdaCaptureItemNamedAst(c1, p1, p2, p3, p4)

    # ===== TYPES =====

    @parser_rule
    @tested_parser_rule
    def parse_type(self) -> TypeAst:
        p1 = self.parse_type_union().for_alt()
        p2 = self.parse_type_tuple().for_alt()
        p3 = self.parse_type_single().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    @tested_parser_rule
    def parse_type_single(self) -> TypeSingleAst:
        c1 = self.current_pos()
        p1 = self.parse_type_namespace().parse_optional()
        p2 = self.parse_type_parts().parse_once()
        return TypeSingleAst(c1, (p1.items if p1 else []) + p2)

    @parser_rule
    @tested_parser_rule
    def parse_type_tuple(self) -> TypeTupleAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_type().parse_zero_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return TypeTupleAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_type_non_union(self) -> TypeAst:
        p1 = self.parse_type_single().for_alt()
        p2 = self.parse_type_tuple().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_type_union(self) -> TypeUnionAst:
        c1 = self.current_pos()
        p1 = self.parse_type_non_union().parse_one_or_more(TokenType.TkBitOr)
        return TypeUnionAst(c1, p1) if len(p1) > 1 else p1[0]

    @parser_rule
    @tested_parser_rule
    def parse_type_namespace(self) -> TypeNamespaceAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_one_or_more(TokenType.TkDot)
        return TypeNamespaceAst(c1, p1)

    @parser_rule
    @tested_parser_rule
    def parse_type_parts(self) -> List[TypePartAst]:
        p1 = self.parse_type_part_first().parse_once()
        p2 = self.parse_type_part().parse_zero_or_more()
        return [p1] + p2

    @parser_rule
    @tested_parser_rule
    def parse_type_part(self) -> TypePartAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkDot).parse_once()
        p2 = self.parse_generic_identifier().for_alt()
        p3 = self.parse_token(TokenType.LxDecDigits).for_alt()
        p4 = (p2 | p3).parse_once()
        return p4

    @parser_rule
    @tested_parser_rule
    def parse_type_part_first(self) -> TypePartAst:
        c1 = self.current_pos()
        p1 = self.parse_generic_identifier().for_alt()
        p2 = self.parse_self_type_keyword().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_self_type_keyword(self) -> GenericIdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwSelfType).parse_once()
        return GenericIdentifierAst(c1, p1.token.token_metadata, None)

    # ===== IDENTIFIERS =====

    @parser_rule
    @tested_parser_rule
    def parse_identifier(self) -> IdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_lexeme(TokenType.LxIdentifier).parse_once()
        return IdentifierAst(c1, p1.token.token_metadata)

    @parser_rule
    @tested_parser_rule
    def parse_upper_identifier(self) -> IdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_lexeme(TokenType.LxUpperIdentifier).parse_once()
        return IdentifierAst(c1, p1.token.token_metadata)

    @parser_rule
    @tested_parser_rule
    def parse_generic_identifier(self) -> GenericIdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_upper_identifier().parse_once()
        p2 = self.parse_generic_arguments().parse_optional()
        return GenericIdentifierAst(c1, p1.value, p2)

    # ===== LITERALS =====

    @parser_rule
    @tested_parser_rule
    def parse_literal(self) -> LiteralAst:
        p1 = self.parse_literal_number().for_alt()
        p2 = self.parse_literal_string().for_alt()
        p3 = self.parse_literal_array().for_alt()
        p4 = self.parse_literal_tuple().for_alt()
        p5 = self.parse_literal_regex().for_alt()
        p6 = self.parse_literal_boolean().for_alt()
        p9 = (p1 | p2 | p3 | p4 | p5 | p6).parse_once()
        return p9

    @parser_rule
    @tested_parser_rule
    def parse_literal_number(self) -> LiteralNumberAst:
        p1 = self.parse_literal_number_b10().for_alt()
        p2 = self.parse_literal_number_b02().for_alt()
        p3 = self.parse_literal_number_b16().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    @tested_parser_rule
    def parse_literal_string(self) -> LiteralStringAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.LxDoubleQuoteStr).parse_once()
        return LiteralStringAst(c1, p1)

    @parser_rule
    @tested_parser_rule
    def parse_literal_array(self) -> LiteralArrayAst:
        p1 = self.parse_literal_array_empty().for_alt()
        p2 = self.parse_literal_array_non_empty().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_literal_tuple(self) -> LiteralTupleAst:
        p1 = self.parse_literal_tuple_0_items().for_alt()
        p2 = self.parse_literal_tuple_1_items().for_alt()
        p3 = self.parse_literal_tuple_n_items().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    @tested_parser_rule
    def parse_literal_regex(self) -> LiteralRegexAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.LxRegex).parse_once()
        return LiteralRegexAst(c1, p1)

    @parser_rule
    @tested_parser_rule
    def parse_literal_boolean(self) -> LiteralBooleanAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwTrue).for_alt()
        p2 = self.parse_token(TokenType.KwFalse).for_alt()
        p3 = (p1 | p2).parse_once()
        return LiteralBooleanAst(c1, p3.token.token_metadata == "true")

    # ===== NUMBERS =====

    @parser_rule
    @tested_parser_rule
    def parse_literal_number_b10(self) -> LiteralNumberBase10Ast:
        c1 = self.current_pos()
        p1 = self.parse_literal_number_b10_float().for_alt()
        p2 = self.parse_literal_number_b10_integer().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_literal_number_b10_integer(self) -> LiteralNumberBase10Ast:
        c1 = self.current_pos()
        p1 = self.parse_numeric_prefix_op().parse_optional()
        p2 = self.parse_token(TokenType.LxDecDigits).parse_once()
        p3 = self.parse_numeric_postfix_type().parse_optional()
        return LiteralNumberBase10Ast(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_literal_number_b10_float(self) -> LiteralNumberBase10Ast:
        c1 = self.current_pos()
        p1 = self.parse_numeric_prefix_op().parse_optional()
        p2 = self.parse_token(TokenType.LxDecFloat).parse_once()
        p3 = self.parse_numeric_postfix_type().parse_optional()
        return LiteralNumberBase10Ast(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_literal_number_b02(self) -> LiteralNumberBase02Ast:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.LxBinDigits).parse_once()
        p2 = self.parse_numeric_postfix_type().parse_optional()
        return LiteralNumberBase02Ast(c1, p1, p2)

    @parser_rule
    @tested_parser_rule
    def parse_literal_number_b16(self) -> LiteralNumberBase16Ast:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.LxHexDigits).parse_once()
        p2 = self.parse_numeric_postfix_type().parse_optional()
        return LiteralNumberBase16Ast(c1, p1, p2)

    @parser_rule
    @tested_parser_rule
    def parse_numeric_prefix_op(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkSub).for_alt()
        p2 = self.parse_token(TokenType.TkAdd).for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    @tested_parser_rule
    def parse_numeric_postfix_type(self) -> TokenType:
        p1 = self.parse_characters("i8").for_alt()
        p2 = self.parse_characters("i16").for_alt()
        p3 = self.parse_characters("i32").for_alt()
        p4 = self.parse_characters("i64").for_alt()
        p5 = self.parse_characters("i128").for_alt()
        p6 = self.parse_characters("i256").for_alt()
        p7 = self.parse_characters("u8").for_alt()
        p8 = self.parse_characters("u16").for_alt()
        p9 = self.parse_characters("u32").for_alt()
        p10 = self.parse_characters("u64").for_alt()
        p11 = self.parse_characters("u128").for_alt()
        p12 = self.parse_characters("u256").for_alt()
        p13 = self.parse_characters("f8").for_alt()
        p14 = self.parse_characters("f16").for_alt()
        p15 = self.parse_characters("f32").for_alt()
        p16 = self.parse_characters("f64").for_alt()
        p17 = self.parse_characters("f128").for_alt()
        p18 = self.parse_characters("f256").for_alt()
        p19 = (p1 | p2 | p3 | p4 | p5 | p6 | p7 | p8 | p9 | p10 | p11 | p12 | p13 | p14 | p15 | p16 | p17 | p18).parse_once()
        return p19

    # ===== ARRAYS =====

    @parser_rule
    @tested_parser_rule
    def parse_literal_array_empty(self) -> LiteralArrayEmptyAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBrackL).parse_once()
        p2 = self.parse_type().parse_once()
        p3 = self.parse_token(TokenType.TkBrackR).parse_once()
        return LiteralArrayEmptyAst(c1, p1, p2, p3)

    @parser_rule
    @tested_parser_rule
    def parse_literal_array_non_empty(self) -> LiteralArrayNonEmptyAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBrackL).parse_once()
        p2 = self.parse_expression().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBrackR).parse_once()
        return LiteralArrayNonEmptyAst(c1, p1, p2, p3)

    # ===== TUPLES =====

    @parser_rule
    @tested_parser_rule
    def parse_literal_tuple_0_items(self) -> LiteralTupleAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_token(TokenType.TkParenR).parse_once()
        return LiteralTupleAst(c1, p1, [], p2)

    @parser_rule
    @tested_parser_rule
    def parse_literal_tuple_1_items(self) -> LiteralTupleAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_expression().parse_once()
        p3 = self.parse_token(TokenType.TkComma).parse_once()
        p4 = self.parse_token(TokenType.TkParenR).parse_once()
        return LiteralTupleAst(c1, p1, [p2], p4)

    @parser_rule
    @tested_parser_rule
    def parse_literal_tuple_n_items(self) -> LiteralTupleAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_expression().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return LiteralTupleAst(c1, p1, p2, p3)

    # ===== TOKENS, KEYWORDS, & LEXEMES =====

    @parser_rule
    @tested_parser_rule
    def parse_lexeme(self, lexeme: TokenType) -> TokenAst:
        p1 = self.parse_token(lexeme).parse_once()
        return p1

    @parser_rule
    @tested_parser_rule
    def parse_characters(self, characters: str) -> TokenAst:
        # TODO : these rules don't come up u the error for failed alternate parsing (see number postfix types)

        p1 = self.parse_identifier().parse_once()
        if p1.value == characters:
            return p1
        else:
            new_error = ParserError(self.current_pos(), f"Expected '{characters}', got '{p1.value}'")
            self._errors.append(new_error)
            raise new_error

    @parser_rule
    @tested_parser_rule
    def parse_token(self, token_type: TokenType) -> TokenAst:
        if token_type == TokenType.NO_TOK:
            return TokenAst(self.current_pos(), Token("", TokenType.NO_TOK))

        if self._index > len(self._tokens) - 1:
            new_error = ParserError(self.current_pos(), f"Expected '{token_type}', got <EOF>")
            self._errors.append(new_error)
            raise new_error

        c1 = self.current_pos()

        while token_type != TokenType.TkNewLine and self.current_tok().token_type in [TokenType.TkNewLine, TokenType.TkWhitespace]:
            self._index += 1
        while token_type == TokenType.TkNewLine and self.current_tok().token_type == TokenType.TkWhitespace:
            self._index += 1

        if self.current_tok().token_type != token_type:
            if any([error.pos == self.current_pos() for error in self._errors]):
                existing_error = next(error for error in self._errors if error.pos == self.current_pos())
                existing_error.expected_tokens.add(token_type.value)
                raise existing_error

            else:
                new_error = ParserError(f"Expected $, got '{self.current_tok().token_metadata}'")
                new_error.pos = self.current_pos()
                new_error.expected_tokens.add(token_type.value)
                self._errors.append(new_error)
                raise new_error

        r = TokenAst(c1, self.current_tok())
        self._index += 1
        return r

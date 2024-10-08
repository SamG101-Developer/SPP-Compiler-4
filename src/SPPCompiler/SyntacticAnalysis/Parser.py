from __future__ import annotations

import functools
from typing import Callable, List, Tuple

from SPPCompiler.LexicalAnalysis.Tokens import Token, TokenType
from SPPCompiler.SyntacticAnalysis.ParserRuleHandler import ParserRuleHandler
from SPPCompiler.SyntacticAnalysis.ParserError import ParserError

from SPPCompiler.Utils.ErrorFormatter import ErrorFormatter
from SPPCompiler.SemanticAnalysis.ASTs import *


# Decorator that wraps the function in a ParserRuleHandler
def parser_rule(func) -> Callable[..., ParserRuleHandler]:
    @functools.wraps(func)
    def wrapper(self, *args) -> ParserRuleHandler:
        return ParserRuleHandler(self, functools.partial(func, self, *args))
    return wrapper


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
            error_message = self._err_fmt.error(final_error.pos, message=error_message, tag_message="Invalid syntax")
            raise SystemExit(error_message) from None

    # ===== PROGRAM =====

    @parser_rule
    def parse_program(self) -> ProgramAst:
        c1 = self.current_pos()
        p1 = self.parse_module_prototype().parse_once()
        p2 = self.parse_eof().parse_once()
        return ProgramAst(c1, p1, p2)

    @parser_rule
    def parse_eof(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkEOF).parse_once()
        return p1

    # ===== MODULES =====

    @parser_rule
    def parse_module_prototype(self) -> ModulePrototypeAst:
        c1 = self.current_pos()
        p1 = []  # self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_module_implementation().parse_once()
        return ModulePrototypeAst(c1, p1, p2)

    @parser_rule
    def parse_module_implementation(self) -> ModuleImplementationAst:
        c1 = self.current_pos()
        p1 = self.parse_module_member().parse_zero_or_more()
        return ModuleImplementationAst(c1, p1)

    @parser_rule
    def parse_module_member(self) -> ModuleMemberAst:
        p1 = self.parse_function_prototype().for_alt()
        p2 = self.parse_class_prototype().for_alt()
        p3 = self.parse_sup_prototype_inheritance().for_alt()
        p4 = self.parse_sup_prototype_normal().for_alt()
        p5 = self.parse_global_use_statement().for_alt()
        p6 = self.parse_global_constant().for_alt()
        p7 = (p1 | p2 | p3 | p4 | p5 | p6).parse_once()
        return p7

    # ===== CLASSES =====

    @parser_rule
    def parse_class_prototype(self) -> ClassPrototypeAst:
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_token(TokenType.KwCls).parse_once()
        p3 = self.parse_upper_identifier().parse_once()
        p4 = self.parse_generic_parameters().parse_optional()
        p5 = self.parse_where_block().parse_optional()
        p6 = self.parse_inner_scope(self.parse_class_attribute).parse_once()
        return ClassPrototypeAst(c1, p1, p2, p3, p4, p5, p6)

    @parser_rule
    def parse_class_attribute(self) -> ClassAttributeAst:
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_identifier().parse_once()
        p3 = self.parse_token(TokenType.TkColon).parse_once()
        p4 = self.parse_type().parse_once()
        return ClassAttributeAst(c1, p1, p2, p3, p4)

    # ===== SUPERIMPOSITION =====

    @parser_rule
    def parse_sup_prototype_normal(self) -> SupPrototypeNormalAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwSup).parse_once()
        p2 = self.parse_generic_parameters().parse_optional()
        p3 = self.parse_type().parse_once()
        p4 = self.parse_where_block().parse_optional()
        p5 = self.parse_inner_scope(self.parse_sup_member).parse_once()
        return SupPrototypeNormalAst(c1, p1, p2, p3, p4, p5)

    @parser_rule
    def parse_sup_prototype_inheritance(self) -> SupPrototypeInheritanceAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwSup).parse_once()
        p2 = self.parse_generic_parameters().parse_optional()
        p3 = self.parse_type().parse_once()
        p4 = self.parse_token(TokenType.KwExt).parse_once()
        p5 = self.parse_type().parse_once()
        p6 = self.parse_where_block().parse_optional()
        p7 = self.parse_inner_scope(self.parse_sup_member).parse_once()
        return SupPrototypeInheritanceAst(c1, p1, p2, p3, p6, p7, p4, p5)

    @parser_rule
    def parse_sup_member(self) -> SupMemberAst:
        p1 = self.parse_sup_method_prototype().parse_once()
        # p2 = self.parse_sup_use_statement().for_alt()
        # p3 = (p1 | p2).parse_once()
        return p1

    # @parser_rule
    # def parse_sup_use_statement(self) -> SupTypedefAst:
    #     p1 = self.parse_annotation().parse_zero_or_more()
    #     p2 = self.parse_typedef_statement().parse_once()
    #     return SupTypedefAst(**p2.__dict__, annotations=p1)

    @parser_rule
    def parse_sup_method_prototype(self) -> FunctionPrototypeAst:
        p1 = self.parse_function_prototype().parse_once()
        return p1

    # ===== FUNCTIONS =====

    @parser_rule
    def parse_function_prototype(self) -> FunctionPrototypeAst:
        p1 = self.parse_subroutine_prototype().for_alt()
        p2 = self.parse_coroutine_prototype().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_subroutine_prototype(self) -> SubroutinePrototypeAst:
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
        return SubroutinePrototypeAst(c1, p1, p2, p3, p4, p5, p6, p7, p8, p9)

    @parser_rule
    def parse_coroutine_prototype(self) -> CoroutinePrototypeAst:
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_token(TokenType.KwCor).parse_once()
        p3 = self.parse_identifier().parse_once()
        p4 = self.parse_generic_parameters().parse_optional()
        p5 = self.parse_function_parameters().parse_once()
        p6 = self.parse_token(TokenType.TkArrowR).parse_once()
        p7 = self.parse_type().parse_once()
        p8 = self.parse_where_block().parse_optional()
        p9 = self.parse_inner_scope(self.parse_statement).parse_once()
        return CoroutinePrototypeAst(c1, p1, p2, p3, p4, p5, p6, p7, p8, p9)

    @parser_rule
    def parse_function_call_arguments(self) -> FunctionArgumentGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_function_call_argument().parse_zero_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return FunctionArgumentGroupAst(c1, p1, p2, p3)

    @parser_rule
    def parse_function_call_argument(self) -> FunctionArgumentAst:
        p1 = self.parse_function_call_argument_named().for_alt()
        p2 = self.parse_function_call_argument_normal().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_function_call_argument_normal(self) -> FunctionArgumentNormalAst:
        c1 = self.current_pos()
        p1 = self.parse_convention().parse_once()
        p2 = self.parse_token(TokenType.TkVariadic).parse_optional()
        p3 = self.parse_expression().parse_once()
        return FunctionArgumentNormalAst(c1, p1, p2, p3)

    @parser_rule
    def parse_function_call_argument_named(self) -> FunctionArgumentNamedAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_convention().parse_once()
        p4 = self.parse_expression().parse_once()
        return FunctionArgumentNamedAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_function_parameters(self) -> FunctionParameterGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_function_parameter().parse_zero_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return FunctionParameterGroupAst(c1, p1, p2, p3)

    @parser_rule
    def parse_function_parameter(self) -> FunctionParameterAst:
        p1 = self.parse_function_parameter_variadic().for_alt()
        p2 = self.parse_function_parameter_optional().for_alt()
        p3 = self.parse_function_parameter_required().for_alt()
        p4 = self.parse_function_parameter_self().for_alt()
        p5 = (p1 | p2 | p3 | p4).parse_once()
        return p5

    @parser_rule
    def parse_function_parameter_self(self) -> FunctionParameterSelfAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwMut).parse_optional()
        p2 = self.parse_convention().parse_once()
        p3 = self.parse_self_keyword().parse_once()
        return FunctionParameterSelfAst(c1, p1, p2, p3)

    @parser_rule
    def parse_function_parameter_required(self) -> FunctionParameterRequiredAst:
        c1 = self.current_pos()
        p1 = self.parse_local_variable().parse_once()
        p2 = self.parse_token(TokenType.TkColon).parse_once()
        p3 = self.parse_convention().parse_once()
        p4 = self.parse_type().parse_once()
        return FunctionParameterRequiredAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_function_parameter_optional(self) -> FunctionParameterOptionalAst:
        c1 = self.current_pos()
        p1 = self.parse_local_variable().parse_once()
        p2 = self.parse_token(TokenType.TkColon).parse_once()
        p3 = self.parse_convention().parse_once()
        p4 = self.parse_type().parse_once()
        p5 = self.parse_token(TokenType.TkAssign).parse_once()
        p6 = self.parse_expression().parse_once()
        return FunctionParameterOptionalAst(c1, p1, p2, p3, p4, p5, p6)

    @parser_rule
    def parse_function_parameter_variadic(self) -> FunctionParameterVariadicAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkVariadic).parse_once()
        p2 = self.parse_local_variable().parse_once()
        p3 = self.parse_token(TokenType.TkColon).parse_once()
        p4 = self.parse_convention().parse_once()
        p5 = self.parse_type().parse_once()
        return FunctionParameterVariadicAst(c1, p1, p2, p3, p4, p5)

    # ===== GENERICS =====

    @parser_rule
    def parse_generic_arguments(self) -> GenericArgumentGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBrackL).parse_once()
        p2 = self.parse_generic_argument().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBrackR).parse_once()
        return GenericArgumentGroupAst(c1, p1, p2, p3)

    @parser_rule
    def parse_generic_argument(self) -> GenericArgumentAst:
        p1 = self.parse_generic_argument_named().for_alt()
        p2 = self.parse_generic_argument_normal().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_generic_argument_normal(self) -> GenericArgumentNormalAst:
        c1 = self.current_pos()
        p1 = self.parse_type().parse_once()
        return GenericArgumentNormalAst(c1, p1)

    @parser_rule
    def parse_generic_argument_named(self) -> GenericArgumentNamedAst:
        c1 = self.current_pos()
        p1 = self.parse_upper_identifier().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_type().parse_once()
        return GenericArgumentNamedAst(c1, p1, p2, p3)

    @parser_rule
    def parse_generic_parameters(self) -> GenericParameterGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBrackL).parse_once()
        p2 = self.parse_generic_parameter().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBrackR).parse_once()
        return GenericParameterGroupAst(c1, p1, p2, p3)

    @parser_rule
    def parse_generic_parameter(self) -> GenericParameterAst:
        p1 = self.parse_generic_parameter_variadic().for_alt()
        p2 = self.parse_generic_parameter_optional().for_alt()
        p3 = self.parse_generic_parameter_required().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    def parse_generic_parameter_required(self) -> GenericParameterRequiredAst:
        c1 = self.current_pos()
        p1 = self.parse_upper_identifier().parse_once()
        p2 = self.parse_generic_inline_constraints().parse_optional()
        return GenericParameterRequiredAst(c1, p1, p2)

    @parser_rule
    def parse_generic_parameter_optional(self) -> GenericParameterOptionalAst:
        c1 = self.current_pos()
        p1 = self.parse_upper_identifier().parse_once()
        p2 = self.parse_generic_inline_constraints().parse_optional()
        p3 = self.parse_token(TokenType.TkAssign).parse_once()
        p4 = self.parse_type().parse_once()
        return GenericParameterOptionalAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_generic_parameter_variadic(self) -> GenericParameterVariadicAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkVariadic).parse_once()
        p2 = self.parse_upper_identifier().parse_once()
        p3 = self.parse_generic_inline_constraints().parse_optional()
        return GenericParameterVariadicAst(c1, p1, p2, p3)

    @parser_rule
    def parse_generic_inline_constraints(self) -> GenericParameterInlineConstraintAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkColon).parse_once()
        p2 = self.parse_type().parse_one_or_more(TokenType.TkAdd)
        return GenericParameterInlineConstraintAst(c1, p1, p2)

    # ===== WHERE =====

    @parser_rule
    def parse_where_block(self) -> WhereBlockAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwWhere).parse_once()
        p2 = self.parse_where_block_constraints_group().parse_once()
        return WhereBlockAst(c1, p1, p2)

    @parser_rule
    def parse_where_block_constraints_group(self) -> WhereConstraintsGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBrackL).parse_once()
        p2 = self.parse_where_block_constraints().parse_zero_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBrackR).parse_once()
        return WhereConstraintsGroupAst(c1, p1, p2, p3)

    @parser_rule
    def parse_where_block_constraints(self) -> WhereConstraintsAst:
        c1 = self.current_pos()
        p1 = self.parse_type().parse_one_or_more(TokenType.TkComma)
        p2 = self.parse_token(TokenType.TkColon).parse_once()
        p3 = self.parse_type().parse_once()
        return WhereConstraintsAst(c1, p1, p2, p3)

    # ===== ANNOTATIONS =====

    @parser_rule
    def parse_annotation(self) -> AnnotationAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkAt).parse_once()
        p2 = self.parse_identifier().parse_one_or_more(TokenType.TkDblColon)
        # p3 = self.parse_postfix_op_function_call().parse_once()
        return AnnotationAst(c1, p1, p2)  # , p3)

    # ===== EXPRESSIONS =====

    @parser_rule
    def parse_expression(self) -> ExpressionAst:
        p1 = self.parse_binary_expression_precedence_level_1().parse_once()
        return p1

    @parser_rule
    def parse_binary_expression_precedence_level_n_rhs(self, op, rhs) -> Tuple[TokenAst, ExpressionAst]:
        p1 = op().parse_once()
        p2 = rhs().parse_once()
        return p1, p2

    @parser_rule
    def parse_binary_expression_precedence_level_n(self, lhs, op, rhs) -> BinaryExpressionAst:
        c1 = self.current_pos()
        p1 = lhs().parse_once()
        p2 = self.parse_binary_expression_precedence_level_n_rhs(op, rhs).parse_optional()
        return BinaryExpressionAst(c1, p1, p2[0], p2[1]) if p2 else p1

    def parse_binary_expression_precedence_level_1(self) -> ParserRuleHandler:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_2,
            self.parse_binary_op_precedence_level_1,
            self.parse_binary_expression_precedence_level_1)

    def parse_binary_expression_precedence_level_2(self) -> ParserRuleHandler:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_3,
            self.parse_binary_op_precedence_level_2,
            self.parse_binary_expression_precedence_level_2)

    def parse_binary_expression_precedence_level_3(self) -> ParserRuleHandler:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_4,
            self.parse_binary_op_precedence_level_3,
            self.parse_pattern_variant_object_destructure)

    def parse_binary_expression_precedence_level_4(self) -> ParserRuleHandler:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_5,
            self.parse_binary_op_precedence_level_4,
            self.parse_binary_expression_precedence_level_4)

    def parse_binary_expression_precedence_level_5(self) -> ParserRuleHandler:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_6,
            self.parse_binary_op_precedence_level_5,
            self.parse_binary_expression_precedence_level_5)

    def parse_binary_expression_precedence_level_6(self) -> ParserRuleHandler:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_unary_expression,
            self.parse_binary_op_precedence_level_6,
            self.parse_binary_expression_precedence_level_6)

    @parser_rule
    def parse_unary_expression(self) -> ExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_unary_op().parse_zero_or_more()
        p2 = self.parse_postfix_expression().parse_once()
        return functools.reduce(lambda acc, x: UnaryExpressionAst(c1, x, acc), p1, p2)

    @parser_rule
    def parse_postfix_expression(self) -> ExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_primary_expression().parse_once()
        p2 = self.parse_postfix_op().parse_zero_or_more()
        return functools.reduce(lambda acc, x: PostfixExpressionAst(c1, acc, x), p2, p1)

    @parser_rule
    def parse_primary_expression(self) -> ExpressionAst:
        p1 = self.parse_literal().for_alt()
        p2 = self.parse_object_initialization().for_alt()
        p3 = self.parse_lambda_prototype().for_alt()
        p4 = self.parse_parenthesized_expression().for_alt()
        p5 = self.parse_identifier().for_alt()
        p6 = self.parse_type().for_alt()
        p7 = self.parse_case_expression().for_alt()
        p8 = self.parse_loop_expression().for_alt()
        p9 = self.parse_gen_expression().for_alt()
        p10 = self.parse_with_expression().for_alt()
        p11 = self.parse_inner_scope(self.parse_statement).for_alt()
        p12 = self.parse_self_keyword().for_alt()
        p13 = self.parse_token(TokenType.TkVariadic).for_alt()
        p14 = (p1 | p2 | p3 | p4 | p5 | p6 | p7 | p8 | p9 | p10 | p11 | p12 | p13).parse_once()
        return p14

    @parser_rule
    def parse_parenthesized_expression(self) -> ParenthesizedExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_expression().parse_once()
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return ParenthesizedExpressionAst(c1, p1, p2, p3)

    @parser_rule
    def parse_self_keyword(self) -> IdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwSelf).parse_once()
        return IdentifierAst(c1, p1.token.token_metadata)

    # ===== EXPRESSION STATEMENTS =====

    @parser_rule
    def parse_case_expression(self) -> CaseExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwCase).parse_once()
        p2 = self.parse_expression().parse_once()
        p3 = self.parse_token(TokenType.KwThen).parse_optional()
        p4 = self.parse_pattern_statement().parse_one_or_more()
        return CaseExpressionAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_loop_expression(self) -> LoopExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwLoop).parse_once()
        p2 = self.parse_loop_expression_condition().parse_once()
        p3 = self.parse_inner_scope(self.parse_statement).parse_once()
        p4 = self.parse_loop_else_expression().parse_optional()
        return LoopExpressionAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_loop_expression_condition(self) -> LoopExpressionConditionAst:
        p1 = self.parse_loop_expression_condition_iterable().for_alt()
        p2 = self.parse_loop_expression_condition_boolean().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_loop_expression_condition_boolean(self) -> LoopExpressionConditionBooleanAst:
        c1 = self.current_pos()
        p1 = self.parse_expression().parse_once()
        return LoopExpressionConditionBooleanAst(c1, p1)

    @parser_rule
    def parse_loop_expression_condition_iterable(self) -> LoopExpressionConditionIterableAst:
        c1 = self.current_pos()
        p1 = self.parse_local_variable().parse_once()
        p2 = self.parse_token(TokenType.KwIn).parse_once()
        p3 = self.parse_expression().parse_once()
        return LoopExpressionConditionIterableAst(c1, p1, p2, p3)

    @parser_rule
    def parse_loop_else_expression(self) -> LoopElseExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwElse).parse_once()
        p2 = self.parse_inner_scope(self.parse_statement).parse_once()
        return LoopElseExpressionAst(c1, p1, p2)

    @parser_rule
    def parse_gen_expression(self) -> GenExpressionAst:
        p1 = self.parse_gen_expression_unroll().for_alt()
        p2 = self.parse_gen_expression_normal().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_gen_expression_normal(self) -> GenExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwGen).parse_once()
        p2 = self.parse_convention().parse_once()
        p3 = self.parse_expression().parse_optional()
        return GenExpressionAst(c1, p1, None, p2, p3)

    @parser_rule
    def parse_gen_expression_unroll(self) -> GenExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwGen).parse_once()
        p2 = self.parse_token(TokenType.KwWith).parse_once()
        p3 = self.parse_expression().parse_once()
        return GenExpressionAst(c1, p1, p2, None, p3)

    @parser_rule
    def parse_with_expression(self) -> WithExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwWith).parse_once()
        p2 = self.parse_with_expression_lhs_alias().parse_optional()
        p3 = self.parse_expression().parse_once()
        p4 = self.parse_inner_scope(self.parse_statement).parse_once()
        return WithExpressionAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_with_expression_lhs_alias(self) -> WithExpressionAliasAst:
        c1 = self.current_pos()
        p1 = self.parse_local_variable().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        return WithExpressionAliasAst(c1, p1, p2)

    # ===== STATEMENTS =====

    @parser_rule
    def parse_return_statement(self) -> ReturnStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwRet).parse_once()
        p2 = self.parse_expression().parse_optional()
        return ReturnStatementAst(c1, p1, p2)

    # Todo: Consider "break(2)" instead of "break break"
    @parser_rule
    def parse_exit_statement(self) -> LoopControlFlowStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwExit).parse_one_or_more()
        p2 = self.parse_token(TokenType.KwSkip).for_alt()
        p3 = self.parse_expression().for_alt()
        p4 = (p2 | p3).parse_optional()
        return LoopControlFlowStatementAst(c1, p1, p4)

    @parser_rule
    def parse_skip_statement(self) -> LoopControlFlowStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwSkip).parse_once()
        return LoopControlFlowStatementAst(c1, [], p1)

    @parser_rule
    def parse_pin_statement(self) -> PinStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwPin).parse_once()
        p2 = self.parse_expression().parse_one_or_more(TokenType.TkComma)
        return PinStatementAst(c1, p1, p2)

    @parser_rule
    def parse_rel_statement(self) -> RelStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwRel).parse_once()
        p2 = self.parse_expression().parse_one_or_more(TokenType.TkComma)
        return RelStatementAst(c1, p1, p2)

    @parser_rule
    def parse_inner_scope(self, rule) -> InnerScopeAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBraceL).parse_once()
        p2 = rule().parse_zero_or_more(TokenType.TkNewLine)
        p3 = self.parse_token(TokenType.TkBraceR).parse_once()
        return InnerScopeAst(c1, p1, p2, p3)

    @parser_rule
    def parse_statement(self) -> StatementAst:
        p1 = self.parse_use_statement().for_alt()
        p2 = self.parse_let_statement().for_alt()
        p3 = self.parse_return_statement().for_alt()
        p4 = self.parse_exit_statement().for_alt()
        p5 = self.parse_skip_statement().for_alt()
        p6 = self.parse_pin_statement().for_alt()
        p7 = self.parse_rel_statement().for_alt()
        p8 = self.parse_assignment_statement().for_alt()
        p9 = self.parse_expression().for_alt()
        p10 = (p1 | p2 | p3 | p4 | p5 | p6 | p7 | p8 | p9).parse_once()
        return p10

    # ===== TYPEDEFS =====

    @parser_rule
    def parse_global_use_statement(self) -> UseStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_token(TokenType.KwUse).parse_once()
        p3 = self.parse_use_statement_type_alias().for_alt()
        p4 = self.parse_use_statement_import().for_alt()
        p5 = (p3 | p4).parse_once()
        return UseStatementAst(c1, p1, p2, p5)

    @parser_rule
    def parse_use_statement(self) -> UseStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwUse).parse_once()
        p2 = self.parse_use_statement_type_alias().for_alt()
        p3 = self.parse_use_statement_import().for_alt()
        p4 = (p2 | p3).parse_once()
        return UseStatementAst(c1, [], p1, p4)

    @parser_rule
    def parse_use_statement_import(self) -> UseStatementImportAst:
        c1 = self.current_pos()
        p1 = self.parse_use_statement_import_body().parse_once()
        return UseStatementImportAst(c1, p1)

    @parser_rule
    def parse_use_statement_import_multiple_types(self) -> UseStatementImportMultipleTypesAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_one_or_more(TokenType.TkDblColon)
        p2 = self.parse_token(TokenType.TkBraceL).parse_once()
        p3 = self.parse_use_statement_import_body().parse_one_or_more(TokenType.TkComma)
        p4 = self.parse_token(TokenType.TkBraceR).parse_once()
        return UseStatementImportMultipleTypesAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_use_statement_import_single_type(self) -> UseStatementImportSingleTypeAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_zero_or_more(TokenType.TkDblColon)
        p2 = self.parse_generic_identifier().parse_one_or_more(TokenType.TkDblColon)  # No generics allowed here
        p3 = self.parse_use_statement_import_alias().parse_optional()
        return UseStatementImportSingleTypeAst(c1, p1, p2, p3)

    @parser_rule
    def parse_use_statement_import_alias(self) -> UseStatementImportAliasAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwAs).parse_once()
        p2 = self.parse_upper_identifier().parse_once()
        return UseStatementImportAliasAst(c1, p1, p2)

    @parser_rule
    def parse_use_statement_import_body(self) -> UseStatementImportBodyAst:
        c1 = self.current_pos()
        p1 = self.parse_use_statement_import_multiple_types().for_alt()
        p2 = self.parse_use_statement_import_single_type().for_alt()
        p3 = (p1 | p2).parse_once()
        return UseStatementImportBodyAst(c1, p3)

    @parser_rule
    def parse_use_statement_type_alias(self) -> UseStatementTypeAliasAst:
        c1 = self.current_pos()
        p1 = self.parse_upper_identifier().parse_once()
        p2 = self.parse_generic_parameters().parse_optional()
        p3 = self.parse_token(TokenType.TkAssign).parse_once()
        p4 = self.parse_type().parse_once()
        return UseStatementTypeAliasAst(c1, p1, p2, p3, p4)

    # ===== LET-DECLARATIONS =====

    @parser_rule
    def parse_global_constant(self) -> GlobalConstantAst:
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_token(TokenType.KwLet).parse_once()
        p3 = self.parse_local_variable_single_identifier().parse_once()
        p4 = self.parse_token(TokenType.TkColon).parse_once()
        p5 = self.parse_type().parse_once()
        p6 = self.parse_token(TokenType.TkAssign).parse_once()
        p7 = self.parse_global_constant_value().parse_once()
        return GlobalConstantAst(c1, p1, p2, p3, p4, p5, p6, p7)

    @parser_rule
    def parse_let_statement(self) -> LetStatementAst:
        p1 = self.parse_let_statement_initialized().for_alt()
        p2 = self.parse_let_statement_uninitialized().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_let_statement_initialized(self) -> LetStatementInitializedAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwLet).parse_once()
        p2 = self.parse_local_variable().parse_once()
        p3 = self.parse_token(TokenType.TkAssign).parse_once()
        p4 = self.parse_expression().parse_once()
        return LetStatementInitializedAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_let_statement_uninitialized(self) -> LetStatementUninitializedAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwLet).parse_once()
        p2 = self.parse_local_variable().parse_once()
        p3 = self.parse_token(TokenType.TkColon).parse_once()
        p4 = self.parse_type().parse_once()
        return LetStatementUninitializedAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_local_variable(self) -> LocalVariableAst:
        p1 = self.parse_local_variable_single_identifier().for_alt()
        p2 = self.parse_local_variable_tuple_destructure().for_alt()
        p3 = self.parse_local_variable_object_destructure().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    def parse_local_variable_skip_argument(self) -> LocalVariableSkipArgumentAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkUnderscore).parse_once()
        return LocalVariableSkipArgumentAst(c1, p1)

    @parser_rule
    def parse_local_variable_skip_arguments(self) -> LocalVariableSkipArgumentsAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkVariadic).parse_once()
        p2 = self.parse_local_variable_single_identifier().parse_optional()
        return LocalVariableSkipArgumentsAst(c1, p1, p2)

    @parser_rule
    def parse_local_variable_single_identifier(self) -> LocalVariableSingleIdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwMut).parse_optional()
        p2 = self.parse_identifier().parse_once()
        return LocalVariableSingleIdentifierAst(c1, p1, p2)

    @parser_rule
    def parse_local_variable_object_destructure(self) -> LocalVariableObjectDestructureAst:
        c1 = self.current_pos()
        p1 = self.parse_type_single().parse_once()
        p2 = self.parse_token(TokenType.TkParenL).parse_once()
        p3 = self.parse_local_variable_nested_for_object_destructure().parse_zero_or_more(TokenType.TkComma)
        p4 = self.parse_token(TokenType.TkParenR).parse_once()
        return LocalVariableObjectDestructureAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_local_variable_tuple_destructure(self) -> LocalVariableTupleDestructureAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_local_variable_nested_for_tuple_destructure().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return LocalVariableTupleDestructureAst(c1, p1, p2, p3)

    @parser_rule
    def parse_local_variable_attribute_binding(self) -> LocalVariableAttributeBindingAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_local_variable_nested_for_attribute_binding().parse_once()
        return LocalVariableAttributeBindingAst(c1, p1, p2, p3)

    @parser_rule
    def parse_local_variable_nested_for_object_destructure(self) -> LocalVariableNestedForObjectDestructureAst:
        p1 = self.parse_local_variable_attribute_binding().for_alt()
        p2 = self.parse_local_variable_single_identifier().for_alt()
        p3 = self.parse_local_variable_skip_arguments().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    def parse_local_variable_nested_for_tuple_destructure(self) -> LocalVariableNestedForTupleDestructureAst:
        p1 = self.parse_local_variable_tuple_destructure().for_alt()
        p2 = self.parse_local_variable_object_destructure().for_alt()
        p3 = self.parse_local_variable_single_identifier().for_alt()
        p4 = self.parse_local_variable_skip_arguments().for_alt()
        p5 = self.parse_local_variable_skip_argument().for_alt()
        p6 = (p1 | p2 | p3 | p4 | p5).parse_once()
        return p6

    @parser_rule
    def parse_local_variable_nested_for_attribute_binding(self) -> LocalVariableNestedForAttributeBindingAst:
        p1 = self.parse_local_variable_object_destructure().for_alt()
        p2 = self.parse_local_variable_tuple_destructure().for_alt()
        p3 = self.parse_local_variable_single_identifier().for_alt()  # todo
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    # ===== ASSIGNMENT =====

    @parser_rule
    def parse_assignment_statement(self) -> AssignmentStatementAst:
        # Todo: Investigate multi-assignment: is it work it?
        c1 = self.current_pos()
        p1 = self.parse_expression().parse_one_or_more(TokenType.TkComma)
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_expression().parse_once()
        return AssignmentStatementAst(c1, p1, p2, p3)

    # ===== PATTERNS =====

    @parser_rule
    def parse_pattern_statement(self) -> PatternBlockAst:
        p1 = self.parse_pattern_statement_flavour_destructuring().for_alt()
        p2 = self.parse_pattern_statement_flavour_non_destructuring().for_alt()
        p3 = self.parse_pattern_statement_flavour_else_case().for_alt()
        p4 = self.parse_pattern_statement_flavour_else().for_alt()
        p5 = (p1 | p2 | p3 | p4).parse_once()
        return p5

    @parser_rule
    def parse_pattern_statement_flavour_destructuring(self) -> PatternBlockAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwIs).parse_once()
        p2 = self.parse_pattern_group_destructure().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_pattern_guard().parse_optional()
        p4 = self.parse_inner_scope(self.parse_statement).parse_once()
        return PatternBlockAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_pattern_statement_flavour_non_destructuring(self) -> PatternBlockAst:
        c1 = self.current_pos()
        p1 = self.parse_boolean_comparison_op().parse_once()
        p2 = self.parse_pattern_variant_expression().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_inner_scope(self.parse_statement).parse_once()
        return PatternBlockAst(c1, p1, p2, None, p3)

    @parser_rule
    def parse_pattern_statement_flavour_else(self) -> PatternBlockAst:
        c1 = self.current_pos()
        p1 = self.parse_pattern_variant_else().parse_once()
        p2 = self.parse_inner_scope(self.parse_statement).parse_once()
        return PatternBlockAst(c1, None, [p1], None, p2)

    @parser_rule
    def parse_pattern_statement_flavour_else_case(self) -> PatternBlockAst:
        c1 = self.current_pos()
        p1 = self.parse_pattern_variant_else_case().parse_once()
        return PatternBlockAst(c1, None, [p1], None, None)

    @parser_rule
    def parse_pattern_group_destructure(self) -> PatternGroupDestructureAst:
        p1 = self.parse_pattern_variant_tuple_destructure().for_alt()
        p2 = self.parse_pattern_variant_object_destructure().for_alt()
        return (p1 | p2).parse_once()

    @parser_rule
    def parse_pattern_variant_skip_argument(self) -> PatternVariantSkipArgumentAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkUnderscore).parse_once()
        return PatternVariantSkipArgumentAst(c1, p1)

    @parser_rule
    def parse_pattern_variant_skip_arguments(self) -> PatternVariantSkipArgumentsAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkVariadic).parse_once()
        p2 = self.parse_pattern_variant_single_identifier().parse_optional()
        return PatternVariantSkipArgumentsAst(c1, p1, p2)

    @parser_rule
    def parse_pattern_variant_single_identifier(self) -> PatternVariantSingleIdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwMut).parse_optional()
        p2 = self.parse_identifier().parse_once()
        return PatternVariantSingleIdentifierAst(c1, p1, p2)

    @parser_rule
    def parse_pattern_variant_object_destructure(self) -> PatternVariantObjectDestructureAst:
        c1 = self.current_pos()
        p1 = self.parse_type_single().parse_once()
        p2 = self.parse_token(TokenType.TkParenL).parse_once()
        p3 = self.parse_pattern_variant_nested_for_object_destructure().parse_zero_or_more(TokenType.TkComma)
        p4 = self.parse_token(TokenType.TkParenR).parse_once()
        return PatternVariantObjectDestructureAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_pattern_variant_tuple_destructure(self) -> PatternVariantTupleDestructureAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_pattern_variant_nested_for_tuple_destructure().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return PatternVariantTupleDestructureAst(c1, p1, p2, p3)

    @parser_rule
    def parse_pattern_variant_attribute_binding(self) -> PatternVariantAttributeBindingAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_pattern_variant_nested_for_attribute_binding().parse_once()
        return PatternVariantAttributeBindingAst(c1, p1, p2, p3)

    @parser_rule
    def parse_pattern_variant_literal(self) -> PatternVariantLiteralAst:
        c1 = self.current_pos()
        p1 = self.parse_literal_number().for_alt()
        p2 = self.parse_literal_string().for_alt()
        p3 = self.parse_literal_boolean().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return PatternVariantLiteralAst(c1, p4)

    @parser_rule
    def parse_pattern_variant_expression(self) -> PatternVariantExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_expression().parse_once()
        return PatternVariantExpressionAst(c1, p1)

    @parser_rule
    def parse_pattern_variant_else(self) -> PatternVariantElseAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwElse).parse_once()
        return PatternVariantElseAst(c1, p1)

    @parser_rule
    def parse_pattern_variant_else_case(self) -> PatternVariantElseCaseAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwElse).parse_once()
        p2 = self.parse_case_expression().parse_once()
        return PatternVariantElseCaseAst(c1, p1, p2)

    @parser_rule
    def parse_pattern_variant_nested_for_object_destructure(self) -> PatternVariantNestedForObjectDestructureAst:
        p1 = self.parse_pattern_variant_attribute_binding().for_alt()
        p2 = self.parse_pattern_variant_single_identifier().for_alt()
        p3 = self.parse_pattern_variant_skip_arguments().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    def parse_pattern_variant_nested_for_tuple_destructure(self) -> PatternVariantNestedForTupleDestructureAst:
        p1 = self.parse_pattern_variant_tuple_destructure().for_alt()
        p2 = self.parse_pattern_variant_object_destructure().for_alt()
        p3 = self.parse_pattern_variant_single_identifier().for_alt()
        p4 = self.parse_pattern_variant_literal().for_alt()
        p5 = self.parse_pattern_variant_skip_arguments().for_alt()
        p6 = self.parse_pattern_variant_skip_argument().for_alt()
        p7 = (p1 | p2 | p3 | p4 | p5 | p6).parse_once()
        return p7

    @parser_rule
    def parse_pattern_variant_nested_for_attribute_binding(self) -> PatternVariantNestedForAttributeBindingAst:
        p1 = self.parse_pattern_variant_tuple_destructure().for_alt()
        p2 = self.parse_pattern_variant_object_destructure().for_alt()
        p3 = self.parse_pattern_variant_single_identifier().for_alt()
        p4 = self.parse_pattern_variant_literal().for_alt()
        p5 = (p1 | p2 | p3 | p4).parse_once()
        return p5

    @parser_rule
    def parse_pattern_guard(self) -> PatternGuardAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwAnd).parse_once()
        p2 = self.parse_expression().parse_once()
        return PatternGuardAst(c1, p1, p2)

    # ===== OPERATORS =====

    @parser_rule
    def parse_binary_op_precedence_level_1(self) -> TokenAst:
        p1 = self.parse_token(TokenType.KwOr).parse_once()
        return p1

    @parser_rule
    def parse_binary_op_precedence_level_2(self) -> TokenAst:
        p1 = self.parse_token(TokenType.KwAnd).parse_once()
        return p1

    @parser_rule
    def parse_binary_op_precedence_level_3(self) -> TokenAst:
        p1 = self.parse_token(TokenType.KwIs).parse_once()
        return p1

    @parser_rule
    def parse_binary_op_precedence_level_4(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkEq).for_alt()
        p2 = self.parse_token(TokenType.TkNe).for_alt()
        p3 = self.parse_token(TokenType.TkLe).for_alt()
        p4 = self.parse_token(TokenType.TkGe).for_alt()
        p5 = self.parse_token(TokenType.TkLt).for_alt()
        p6 = self.parse_token(TokenType.TkGt).for_alt()
        p7 = self.parse_token(TokenType.TkSs).for_alt()
        p8 = (p1 | p2 | p3 | p4 | p5 | p6 | p7).parse_once()
        return p8

    @parser_rule
    def parse_binary_op_precedence_level_5(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkAdd).for_alt()
        p2 = self.parse_token(TokenType.TkSub).for_alt()
        p3 = self.parse_token(TokenType.TkAddAssign).for_alt()
        p4 = self.parse_token(TokenType.TkSubAssign).for_alt()
        p5 = (p1 | p2 | p3 | p4).parse_once()
        return p5

    @parser_rule
    def parse_binary_op_precedence_level_6(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkMul).for_alt()
        p2 = self.parse_token(TokenType.TkDiv).for_alt()
        p3 = self.parse_token(TokenType.TkRem).for_alt()
        p4 = self.parse_token(TokenType.TkMod).for_alt()
        p5 = self.parse_token(TokenType.TkExp).for_alt()
        p6 = self.parse_token(TokenType.TkMulAssign).for_alt()
        p7 = self.parse_token(TokenType.TkDivAssign).for_alt()
        p8 = self.parse_token(TokenType.TkRemAssign).for_alt()
        p9 = self.parse_token(TokenType.TkModAssign).for_alt()
        p10 = self.parse_token(TokenType.TkExpAssign).for_alt()
        p11 = (p1 | p2 | p3 | p4 | p5 | p6 | p7 | p8 | p9 | p10).parse_once()
        return p11

    @parser_rule
    def parse_boolean_comparison_op(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkEq).for_alt()
        p2 = self.parse_token(TokenType.TkNe).for_alt()
        p3 = self.parse_token(TokenType.TkLe).for_alt()
        p4 = self.parse_token(TokenType.TkGe).for_alt()
        p5 = self.parse_token(TokenType.TkLt).for_alt()
        p6 = self.parse_token(TokenType.TkGt).for_alt()
        p8 = (p1 | p2 | p3 | p4 | p5 | p6).parse_once()
        return p8

    @parser_rule
    def parse_unary_op(self) -> TokenAst:
        p1 = self.parse_token(TokenType.KwAsync).parse_once()
        return p1

    @parser_rule
    def parse_postfix_op(self) -> PostfixExpressionOperatorAst:
        p1 = self.parse_postfix_op_function_call().for_alt()
        p2 = self.parse_postfix_op_member_access().for_alt()
        p3 = self.parse_postfix_op_early_return().for_alt()
        p4 = self.parse_postfix_op_not_keyword().for_alt()
        p5 = (p1 | p2 | p3 | p4).parse_once()
        return p5

    @parser_rule
    def parse_postfix_op_function_call(self) -> PostfixExpressionOperatorFunctionCallAst:
        c1 = self.current_pos()
        p1 = self.parse_generic_arguments().parse_optional()
        p2 = self.parse_function_call_arguments().parse_once()
        p3 = self.parse_token(TokenType.TkVariadic).parse_optional()
        return PostfixExpressionOperatorFunctionCallAst(c1, p1, p2, p3)

    @parser_rule
    def parse_postfix_op_member_access(self) -> PostfixExpressionOperatorMemberAccessAst:
        p1 = self.parse_postfix_op_member_access_runtime().for_alt()
        p2 = self.parse_postfix_op_member_access_static().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_postfix_op_member_access_runtime(self) -> PostfixExpressionOperatorMemberAccessAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkDot).parse_once()
        p2 = self.parse_identifier().for_alt()
        p3 = self.parse_token(TokenType.LxDecInteger).for_alt()
        p4 = (p2 | p3).parse_once()
        return PostfixExpressionOperatorMemberAccessAst(c1, p1, p4)

    @parser_rule
    def parse_postfix_op_member_access_static(self) -> PostfixExpressionOperatorMemberAccessAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkDblColon).parse_once()
        p2 = self.parse_identifier().parse_once()
        return PostfixExpressionOperatorMemberAccessAst(c1, p1, p2)

    @parser_rule
    def parse_postfix_op_early_return(self) -> PostfixExpressionOperatorEarlyReturnAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkQst).parse_once()
        return PostfixExpressionOperatorEarlyReturnAst(c1, p1)

    @parser_rule
    def parse_postfix_op_not_keyword(self) -> PostfixExpressionOperatorNotKeywordAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkDot).parse_once()
        p2 = self.parse_token(TokenType.KwNot).parse_once()
        return PostfixExpressionOperatorNotKeywordAst(c1, p1, p2)

    # ===== CONVENTIONS =====

    @parser_rule
    def parse_convention(self) -> ConventionAst:
        p1 = self.parse_convention_mut().for_alt()
        p2 = self.parse_convention_ref().for_alt()
        p3 = self.parse_convention_mov().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    def parse_convention_mov(self) -> ConventionMovAst:
        c1 = self.current_pos()
        return ConventionMovAst(c1)

    @parser_rule
    def parse_convention_ref(self) -> ConventionRefAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBorrow).parse_once()
        return ConventionRefAst(c1, p1)

    @parser_rule
    def parse_convention_mut(self) -> ConventionMutAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBorrow).parse_once()
        p2 = self.parse_token(TokenType.KwMut).parse_once()
        return ConventionMutAst(c1, p1, p2)

    # ===== OBJECT INITIALIZATION =====

    @parser_rule
    def parse_object_initialization(self) -> ObjectInitializerAst:
        c1 = self.current_pos()
        p1 = self.parse_type_single().parse_once()
        p2 = self.parse_object_initializer_arguments().parse_once()
        return ObjectInitializerAst(c1, p1, p2)

    @parser_rule
    def parse_object_initializer_arguments(self) -> ObjectInitializerArgumentGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_object_initializer_argument().parse_zero_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return ObjectInitializerArgumentGroupAst(c1, p1, p2, p3)

    @parser_rule
    def parse_object_initializer_argument(self) -> ObjectInitializerArgumentAst:
        p1 = self.parse_object_initializer_argument_named().for_alt()
        p2 = self.parse_object_initializer_argument_normal().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_object_initializer_argument_normal(self) -> ObjectInitializerArgumentNormalAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_once()
        return ObjectInitializerArgumentNormalAst(c1, p1)

    @parser_rule
    def parse_object_initializer_argument_named(self) -> ObjectInitializerArgumentNamedAst:
        c1 = self.current_pos()
        p1 = self.parse_object_initializer_argument_named_key().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_expression().parse_once()
        return ObjectInitializerArgumentNamedAst(c1, p1, p2, p3)

    @parser_rule
    def parse_object_initializer_argument_named_key(self) -> IdentifierAst | TokenAst:
        p1 = self.parse_identifier().for_alt()
        p2 = self.parse_token(TokenType.KwElse).for_alt()
        p3 = self.parse_token(TokenType.KwSup).for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    # ===== LAMBDAS =====

    @parser_rule
    def parse_lambda_prototype(self) -> LambdaPrototypeAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwFun).parse_once()  # todo: allow lambda coroutines
        p2 = self.parse_generic_parameters().parse_optional()
        p3 = self.parse_function_parameters().parse_once()
        p4 = self.parse_token(TokenType.TkArrowR).parse_once()
        p5 = self.parse_type().parse_once()
        p6 = self.parse_lambda_capture_block().parse_optional()
        p7 = self.parse_where_block().parse_optional()
        p8 = self.parse_inner_scope(self.parse_statement).parse_once()
        return LambdaPrototypeAst(c1, p1, p2, p3, p4, p5, p6, p7, p8)

    @parser_rule
    def parse_lambda_capture_block(self) -> LambdaCaptureBlockAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBrackL).parse_once()
        p2 = self.parse_lambda_capture_item().parse_zero_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBrackR).parse_once()
        return LambdaCaptureBlockAst(c1, p1, p2, p3)

    @parser_rule
    def parse_lambda_capture_item(self) -> LambdaCaptureItemAst:
        p1 = self.parse_lambda_capture_item_named().for_alt()
        p2 = self.parse_lambda_capture_item_normal().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_lambda_capture_item_normal(self) -> LambdaCaptureItemNormalAst:
        c1 = self.current_pos()
        p1 = self.parse_convention().parse_once()
        p2 = self.parse_expression().parse_once()
        return LambdaCaptureItemNormalAst(c1, p1, p2)

    @parser_rule
    def parse_lambda_capture_item_named(self) -> LambdaCaptureItemNamedAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_convention().parse_once()
        p4 = self.parse_expression().parse_once()
        return LambdaCaptureItemNamedAst(c1, p1, p2, p3, p4)

    # ===== TYPES =====

    @parser_rule
    def parse_type(self) -> TypeAst:
        p1 = self.parse_type_optional().for_alt()
        p2 = self.parse_type_union().for_alt()
        p3 = self.parse_type_tuple().for_alt()
        p4 = self.parse_type_single().for_alt()
        p5 = (p1 | p2 | p3 | p4).parse_once()
        return p5

    @parser_rule
    def parse_type_optional(self) -> TypeAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkQst).parse_once()
        p2 = self.parse_type().parse_once()
        return TypeOptionalAst(c1, p1, p2).as_single_type()

    @parser_rule
    def parse_type_single(self) -> TypeAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_zero_or_more(TokenType.TkDblColon)
        p2 = self.parse_type_parts().parse_once()
        return TypeAst(c1, p1, p2)

    @parser_rule
    def parse_type_tuple(self) -> TypeAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_type().parse_zero_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        p4 = self.parse_type_part().parse_zero_or_more()
        return TypeTupleAst(c1, p1, p2, p3, p4).as_single_type()

    @parser_rule
    def parse_type_non_union(self) -> TypeAst:
        p1 = self.parse_type_single().for_alt()
        p2 = self.parse_type_tuple().for_alt()
        p3 = self.parse_type_optional().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    def parse_type_union(self) -> TypeAst:
        c1 = self.current_pos()
        p1 = self.parse_type_non_union().parse_one_or_more(TokenType.TkUnion)
        return TypeUnionAst(c1, p1).as_single_type() if len(p1) > 1 else p1[0]

    @parser_rule
    def parse_type_parts(self) -> List[TypePartAst]:
        p1 = self.parse_type_part_first().parse_once()
        p2 = self.parse_type_part().parse_zero_or_more()
        return [p1] + p2

    @parser_rule
    def parse_type_part(self) -> TypePartAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkDblColon).parse_once()
        p2 = self.parse_generic_identifier().for_alt()
        p3 = self.parse_token(TokenType.LxDecInteger).for_alt()
        p4 = (p2 | p3).parse_once()
        return p4

    @parser_rule
    def parse_type_part_first(self) -> TypePartAst:
        c1 = self.current_pos()
        # p1 = self.parse_generic_identifier().for_alt()
        # p2 = self.parse_self_type_keyword().for_alt()
        # p3 = (p1 | p2).parse_once()
        p1 = self.parse_generic_identifier().parse_once()
        return p1

    # @parser_rule
    # def parse_self_type_keyword(self) -> GenericIdentifierAst:
    #     c1 = self.current_pos()
    #     p1 = self.parse_token(TokenType.KwSelfType).parse_once()
    #     return GenericIdentifierAst(c1, p1.token.token_metadata, None)

    # ===== IDENTIFIERS =====

    @parser_rule
    def parse_identifier(self) -> IdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_lexeme(TokenType.LxIdentifier).parse_once()
        return IdentifierAst(c1, p1.token.token_metadata)

    @parser_rule
    def parse_upper_identifier(self) -> IdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_lexeme(TokenType.LxUpperIdentifier).parse_once()
        return IdentifierAst(c1, p1.token.token_metadata)

    @parser_rule
    def parse_generic_identifier(self) -> GenericIdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_upper_identifier().parse_once()
        p2 = self.parse_generic_arguments().parse_optional()
        return GenericIdentifierAst(c1, p1.value, p2)

    # ===== LITERALS =====

    @parser_rule
    def parse_literal(self) -> LiteralAst:
        p1 = self.parse_literal_number().for_alt()
        p2 = self.parse_literal_string().for_alt()
        p3 = self.parse_literal_tuple(self.parse_expression).for_alt()
        p4 = self.parse_literal_array(self.parse_expression).for_alt()
        p5 = self.parse_literal_regex().for_alt()
        p6 = self.parse_literal_boolean().for_alt()
        p7 = (p1 | p2 | p3 | p4 | p5 | p6).parse_once()
        return p7

    @parser_rule
    def parse_literal_number(self) -> NumberLiteralAst:
        p1 = self.parse_literal_number_b10().for_alt()
        p2 = self.parse_literal_number_b02().for_alt()
        p3 = self.parse_literal_number_b16().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    def parse_literal_string(self) -> StringLiteralAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.LxDoubleQuoteStr).parse_once()
        return StringLiteralAst(c1, p1)

    @parser_rule
    def parse_literal_tuple(self, item) -> TupleLiteralAst:
        p1 = self.parse_literal_tuple_0_items().for_alt()
        p2 = self.parse_literal_tuple_1_items(item).for_alt()
        p3 = self.parse_literal_tuple_n_items(item).for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    def parse_literal_array(self, item) -> ArrayLiteralAst:
        p1 = self.parse_literal_array_0_items().for_alt()
        p2 = self.parse_literal_array_n_items(item).for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_literal_regex(self) -> RegexLiteralAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.LxRegex).parse_once()
        return RegexLiteralAst(c1, p1)

    @parser_rule
    def parse_literal_boolean(self) -> BooleanLiteralAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwTrue).for_alt()
        p2 = self.parse_token(TokenType.KwFalse).for_alt()
        p3 = (p1 | p2).parse_once()
        return BooleanLiteralAst(c1, p3.token.token_metadata == "true")

    # ===== NUMBERS =====

    @parser_rule
    def parse_literal_number_b10(self) -> NumberLiteralBase10Ast:
        c1 = self.current_pos()
        p1 = self.parse_literal_number_b10_float().for_alt()
        p2 = self.parse_literal_number_b10_integer().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_literal_number_b10_integer(self) -> NumberLiteralBase10Ast:
        c1 = self.current_pos()
        p1 = self.parse_numeric_prefix_op().parse_optional()
        p2 = self.parse_token(TokenType.LxDecInteger).parse_once()
        p3 = self.parse_numeric_postfix_type().parse_optional()
        return NumberLiteralBase10Ast(c1, p2, p3, p1)

    @parser_rule
    def parse_literal_number_b10_float(self) -> NumberLiteralBase10Ast:
        c1 = self.current_pos()
        p1 = self.parse_numeric_prefix_op().parse_optional()
        p2 = self.parse_token(TokenType.LxDecDecimal).parse_once()
        p3 = self.parse_numeric_postfix_type().parse_optional()
        return NumberLiteralBase10Ast(c1, p1, p2, p3)

    @parser_rule
    def parse_literal_number_b02(self) -> NumberLiteralBase02Ast:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.LxBinDigits).parse_once()
        p2 = self.parse_numeric_postfix_type().parse_optional()
        return NumberLiteralBase02Ast(c1, p1, p2)

    @parser_rule
    def parse_literal_number_b16(self) -> NumberLiteralBase16Ast:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.LxHexDigits).parse_once()
        p2 = self.parse_numeric_postfix_type().parse_optional()
        return NumberLiteralBase16Ast(c1, p1, p2)

    @parser_rule
    def parse_numeric_prefix_op(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkSub).for_alt()
        p2 = self.parse_token(TokenType.TkAdd).for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_numeric_postfix_type(self) -> TokenType:
        p1  = self.parse_token(TokenType.TkUnderscore).parse_once()
        p2  = self.parse_characters("i8").for_alt()
        p3  = self.parse_characters("i16").for_alt()
        p4  = self.parse_characters("i32").for_alt()
        p5  = self.parse_characters("i64").for_alt()
        p6  = self.parse_characters("i128").for_alt()
        p7  = self.parse_characters("i256").for_alt()
        p8  = self.parse_characters("u8").for_alt()
        p9  = self.parse_characters("u16").for_alt()
        p10 = self.parse_characters("u32").for_alt()
        p11 = self.parse_characters("u64").for_alt()
        p12 = self.parse_characters("u128").for_alt()
        p13 = self.parse_characters("u256").for_alt()
        p14 = self.parse_characters("f8").for_alt()
        p15 = self.parse_characters("f16").for_alt()
        p16 = self.parse_characters("f32").for_alt()
        p17 = self.parse_characters("f64").for_alt()
        p18 = self.parse_characters("f128").for_alt()
        p19 = self.parse_characters("f256").for_alt()
        p20 = (p2 | p3 | p4 | p5 | p6 | p7 | p8 | p9 | p10 | p11 | p12 | p13 | p14 | p15 | p16 | p17 | p18 | p19).parse_once()
        return p20

    # ===== TUPLES =====

    @parser_rule
    def parse_literal_tuple_0_items(self) -> TupleLiteralAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_token(TokenType.TkParenR).parse_once()
        return TupleLiteralAst(c1, p1, [], p2)

    @parser_rule
    def parse_literal_tuple_1_items(self, item) -> TupleLiteralAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = item().parse_once()
        p3 = self.parse_token(TokenType.TkComma).parse_once()
        p4 = self.parse_token(TokenType.TkParenR).parse_once()
        return TupleLiteralAst(c1, p1, [p2], p4)

    @parser_rule
    def parse_literal_tuple_n_items(self, item) -> TupleLiteralAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = item().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return TupleLiteralAst(c1, p1, p2, p3)

    # ===== ARRAYS =====

    @parser_rule
    def parse_literal_array_0_items(self) -> ArrayLiteral0ElementAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBrackL).parse_once()
        p2 = self.parse_type().parse_once()
        p3 = self.parse_token(TokenType.TkBrackR).parse_once()
        return ArrayLiteral0ElementAst(c1, p1, p2, p3)

    @parser_rule
    def parse_literal_array_n_items(self, item) -> ArrayLiteralNElementAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBrackL).parse_once()
        p2 = item().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBrackR).parse_once()
        return ArrayLiteralNElementAst(c1, p1, p2, p3)

    # ===== GLOBAL CONSTANTS =====
    @parser_rule
    def parse_global_constant_value(self) -> ExpressionAst:
        p1 = self.parse_literal_number().for_alt()
        p2 = self.parse_literal_string().for_alt()
        p3 = self.parse_literal_tuple(self.parse_global_constant_value).for_alt()
        p4 = self.parse_literal_array(self.parse_global_constant_value).for_alt()
        p5 = self.parse_literal_regex().for_alt()
        p6 = self.parse_literal_boolean().for_alt()
        p7 = self.parse_global_object_initialization().for_alt()
        p8 = (p1 | p2 | p3 | p4 | p5 | p6 | p7).parse_once()
        return p8

    @parser_rule
    def parse_global_object_initialization(self) -> ObjectInitializerAst:
        c1 = self.current_pos()
        p1 = self.parse_type_single().parse_once()
        p2 = self.parse_global_object_initializer_arguments().parse_once()
        return ObjectInitializerAst(c1, p1, p2)

    @parser_rule
    def parse_global_object_initializer_arguments(self) -> ObjectInitializerArgumentGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_global_object_initializer_argument_named().parse_zero_or_more(TokenType.TkComma)  # todo: normal too for copyables?
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return ObjectInitializerArgumentGroupAst(c1, p1, p2, p3)

    @parser_rule
    def parse_global_object_initializer_argument_named(self) -> ObjectInitializerArgumentNamedAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_global_constant_value().parse_once()
        return ObjectInitializerArgumentNamedAst(c1, p1, p2, p3)

    # ===== TOKENS, KEYWORDS, & LEXEMES =====

    @parser_rule
    def parse_lexeme(self, lexeme: TokenType) -> TokenAst:
        p1 = self.parse_token(lexeme).parse_once()
        return p1

    @parser_rule
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
    def parse_token(self, token_type: TokenType) -> TokenAst:
        # For the "no token", instantly return a new token.
        if token_type == TokenType.NO_TOK:
            return TokenAst(self.current_pos(), Token("", TokenType.NO_TOK))

        # Check if the end of the file has been reached.
        if self._index > len(self._tokens) - 1:
            new_error = ParserError(self.current_pos(), f"Expected '{token_type}', got <EOF>")
            self._errors.append(new_error)
            raise new_error

        # Save the current position before skipping newlines and whitespace.
        c1 = self.current_pos()

        # Skip newlines and whitespace for non-newline parsing, and whitespace only for new-line parsing.
        while token_type != TokenType.TkNewLine and self.current_tok().token_type in [TokenType.TkNewLine, TokenType.TkWhitespace]:
            self._index += 1
        while token_type == TokenType.TkNewLine and self.current_tok().token_type == TokenType.TkWhitespace:
            self._index += 1

        # Handle an incorrectly placed token.
        if self.current_tok().token_type != token_type:
            token_print = lambda t: f"<{t.name[2:]}>" if t.name.startswith("Lx") else t.value

            c2 = self.current_pos()
            for error in self._errors:
                if error.pos == c2:
                    existing_error = next(error for error in self._errors if error.pos == c2)
                    existing_error.expected_tokens.add(token_print(token_type))
                    raise existing_error

            new_error = ParserError(f"Expected $, got '{token_print(self.current_tok().token_type)}'")
            new_error.pos = c2
            new_error.expected_tokens.add(token_print(token_type))
            self._errors.append(new_error)
            raise new_error

        # Otherwise, the parse was successful, to return a TokenAst as the correct position.
        r = TokenAst(c1, self.current_tok())
        self._index += 1
        return r


__all__ = ["Parser", "parser_rule"]

from __future__ import annotations

import functools
from typing import Callable, Tuple

from SyntacticAnalysis.ParserRuleHandler import ParserRuleHandler
from src.LexicalAnalysis.Tokens import Token, TokenType
from src.SyntacticAnalysis.ParserRuleHandler import ParserRuleHandler
from src.SemanticAnalysis.ASTs.Ast import *


# Decorator that wraps the function in a ParserRuleHandler
def parser_rule[T](rule: ParserRuleHandler.ParserRule[T]) -> Callable[[], ParserRuleHandler[T]]:
    def wrapper(self, *args) -> ParserRuleHandler[T]:
        return ParserRuleHandler(self, rule)
    return wrapper


class Parser:
    _tokens: List[Token]
    _index: int

    def current_pos(self) -> int:
        return self._index

    # ===== PROGRAM =====

    @parser_rule
    def parse_eof(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkEOF).parse_once()
        return p1

    @parser_rule
    def parse_program(self) -> ProgramAst:
        c1 = self.current_pos()
        p1 = self.parse_module_prototype().parse_once()
        p2 = self.parse_eof().parse_once()
        return ProgramAst(c1, p1, p2)

    # ===== MODULES =====

    @parser_rule
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
        p4 = self.parse_module_member().parse_zero_or_more()
        return ModulePrototypeAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_module_member(self) -> ModuleMemberAst:
        p1 = self.parse_function_prototype().for_alt()
        p2 = self.parse_class_prototype().for_alt()
        p3 = self.parse_sup_prototype_normal().for_alt()
        p4 = self.parse_sup_prototype_inheritance().for_alt()
        p5 = self.parse_typedef_statement().for_alt()
        p6 = (p1 | p2 | p3 | p4 | p5).parse_once()
        return p6

    @parser_rule
    def parse_module_identifier(self) -> ModuleIdentifierAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_one_or_more(TokenType.TkDot)
        return ModuleIdentifierAst(c1, p1)

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
        p4 = self.parse_token(TokenType.KwOn).parse_once()
        p5 = self.parse_type().parse_once()
        p6 = self.parse_where_block().parse_optional()
        p7 = self.parse_inner_scope(self.parse_sup_member).parse_once()
        return SupPrototypeInheritanceAst(c1, p3, p4, p5, p6, p7, p1, p2)

    @parser_rule
    def parse_sup_member(self) -> SupMemberAst:
        p1 = self.parse_sup_method_prototype().for_alt()
        p2 = self.parse_sup_typedef().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_sup_typedef(self) -> SupTypedefAst:
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_statement_typedef().parse_once()
        return SupTypedefAst(**p2.__dict__, pos=c1, annotations=p1)

    @parser_rule
    def parse_sup_method_prototype(self) -> SupMethodPrototypeAst:
        p1 = self.parse_function_prototype().parse_once()
        return SupMethodPrototypeAst(**p1.__dict__)

    # ===== FUNCTIONS =====

    @parser_rule
    def parse_function_prototype(self) -> FunctionPrototypeAst:
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_token(TokenType.KwFn).parse_once()
        p3 = self.parse_identifier().parse_once()
        p4 = self.parse_generic_parameters().parse_optional()
        p5 = self.parse_function_parameters().parse_once()
        p6 = self.parse_token(TokenType.TkArrowR).parse_once()
        p7 = self.parse_type().parse_once()
        p8 = self.parse_where_block().parse_optional()
        p9 = self.parse_inner_scope(self.parse_statement).parse_once()
        return FunctionPrototypeAst(c1, p1, p2, p3, p4, p5, p6, p7, p8, p9)

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
        p3 = self.parse_token(TokenType.KwSelf).parse_once()
        return FunctionParameterSelfAst(c1, p1, p2, p3)

    @parser_rule
    def parse_function_parameter_required(self) -> FunctionParameterRequiredAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwMut).parse_optional()
        p2 = self.parse_identifier().parse_once()
        p3 = self.parse_token(TokenType.TkColon).parse_once()
        p4 = self.parse_convention().parse_once()
        p5 = self.parse_type().parse_once()
        return FunctionParameterRequiredAst(c1, p1, p2, p3, p4, p5)

    @parser_rule
    def parse_function_parameter_optional(self) -> FunctionParameterOptionalAst:
        p1 = self.parse_function_parameter_required().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_expression().parse_once()
        return FunctionParameterOptionalAst(**p1.__dict__, assignment_token=p2, default_value=p3)

    @parser_rule
    def parse_function_parameter_variadic(self) -> FunctionParameterVariadicAst:
        p1 = self.parse_token(TokenType.TkVariadic).parse_optional()
        p2 = self.parse_function_parameter_required().parse_once()
        return FunctionParameterVariadicAst(**p2.__dict__, variadic_token=p1)

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
        p3 = self.parse_generic_argument_normal().parse_once()
        return GenericArgumentNamedAst(c1, p1, p2, **p3.__dict__)

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
        p1 = self.parse_generic_parameter_required().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_type().parse_once()
        return GenericParameterOptionalAst(**p1.__dict__, assignment_token=p2, default_value=p3)

    @parser_rule
    def parse_generic_parameter_variadic(self) -> GenericParameterVariadicAst:
        p1 = self.parse_token(TokenType.TkVariadic).parse_optional()
        p2 = self.parse_generic_parameter_required().parse_once()
        return GenericParameterVariadicAst(**p2.__dict__, variadic_token=p1)

    # ===== WHERE =====

    @parser_rule
    def parse_where_block(self) -> WhereBlockAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwWhere).parse_once()
        p2 = self.parse_where_block_constraints_group().parse_optional()
        return WhereBlockAst(c1, p1, p2)

    @parser_rule
    def parse_where_block_constraints_group(self) -> WhereConstraintsGroupAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBraceL).parse_once()
        p2 = self.parse_where_block_constraints().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBraceR).parse_once()
        return WhereConstraintsGroupAst(c1, p1, p2, p3)

    @parser_rule
    def parse_where_block_constraints(self) -> WhereConstraintsAst:
        c1 = self.current_pos()
        p1 = self.parse_type().parse_one_or_more(TokenType.TkComma)
        p2 = self.parse_token(TokenType.TkColon).parse_once()
        p3 = self.parse_type().parse_one_or_more(TokenType.TkBitAnd)

    # ===== ANNOTATIONS =====

    @parser_rule
    def parse_annotations(self) -> AnnotationAst:
        raise NotImplementedError("Annotations won't be supported until compiler is self-hosting.")

    # ===== EXPRESSIONS =====

    @parser_rule
    def parse_expression(self) -> ExpressionAst:
        p1 = self.parse_binary_expression_precedence_level_1().parse_once()
        return p1

    @parser_rule
    def parse_binary_expression_precedence_level_n_rhs(self, op, rhs) -> Tuple[TokenAst, ExpressionAst]:
        p1 = op().parse_once()
        p2 = rhs().parse_once()
        return (p1, p2)

    @parser_rule
    def parse_binary_expression_precedence_level_n(self, lhs, op, rhs) -> BinaryExpressionAst:
        c1 = self.current_pos()
        p1 = lhs().parse_once()
        p2 = self.parse_binary_expression_precedence_level_n_rhs(op, rhs).parse_optional()
        return BinaryExpressionAst(c1, p1, p2[0], p2[1]) if p2 else p1

    @parser_rule
    def parse_binary_expression_precedence_level_1(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_2,
            self.parse_binary_op_precedence_level_1,
            self.parse_binary_expression_precedence_level_1)

    @parser_rule
    def parse_binary_expression_precedence_level_2(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_3,
            self.parse_binary_op_precedence_level_2,
            self.parse_binary_expression_precedence_level_2)

    @parser_rule
    def parse_binary_expression_precedence_level_3(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_4,
            self.parse_binary_op_precedence_level_3,
            self.parse_binary_expression_precedence_level_3)

    @parser_rule
    def parse_binary_expression_precedence_level_4(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_5,
            self.parse_binary_op_precedence_level_4,
            self.parse_binary_expression_precedence_level_4)

    @parser_rule
    def parse_binary_expression_precedence_level_5(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_6,
            self.parse_binary_op_precedence_level_5,
            self.parse_binary_expression_precedence_level_5)

    @parser_rule
    def parse_binary_expression_precedence_level_6(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_binary_expression_precedence_level_7,
            self.parse_binary_op_precedence_level_6,
            self.parse_binary_expression_precedence_level_6)

    @parser_rule
    def parse_binary_expression_precedence_level_7(self) -> ExpressionAst:
        return self.parse_binary_expression_precedence_level_n(
            self.parse_unary_expression,
            self.parse_binary_op_precedence_level_7,
            self.parse_binary_expression_precedence_level_7)

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
    def parse_if_expression(self) -> IfExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwIf).parse_once()
        p2 = self.parse_expression().parse_once()
        p3 = self.parse_pattern_comp_op().parse_optional()
        p4 = self.parse_pattern_statement().parse_one_or_more()
        return IfExpressionAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_while_expression(self) -> WhileExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwWhile).parse_once()
        p2 = self.parse_expression().parse_once()
        p3 = self.parse_inner_scope(self.parse_statement).parse_once()
        p4 = self.parse_residual_block().parse_optional()
        return WhileExpressionAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_yield_expression(self) -> YieldExpressionAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwYield).parse_once()
        p2 = self.parse_convention().parse_once()
        p3 = self.parse_expression().parse_once()
        return YieldExpressionAst(c1, p1, p2, p3)

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
        p1 = self.parse_local_variable_single().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        return WithExpressionAliasAst(c1, p1, p2)

    # ===== STATEMENTS =====

    @parser_rule
    def parse_return_statement(self) -> ReturnStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwRet).parse_once()
        p2 = self.parse_expression().parse_optional()
        return ReturnStatementAst(c1, p1, p2)

    @parser_rule
    def parse_typedef_statement(self) -> TypedefStatementAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwUse).parse_once()
        p2 = self.parse_generic_parameters().parse_optional()
        p3 = self.parse_type_namespace().parse_once()
        p4 = self.parse_typedef_item().parse_once()
        return TypedefStatementAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_typedef_item(self) -> TypedefStatementItemAst:
        p1 = self.parse_typedef_statement_specific_item().for_alt()
        p1 = self.parse_typedef_statement_specific_items().for_alt()
        p2 = self.parse_typedef_statement_all_items().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_typedef_statement_specific_item(self) -> TypedefStatementSpecificItemAst:
        c1 = self.current_pos()
        p1 = self.parse_type_single().parse_once()
        p2 = self.parse_typedef_statement_specific_item_alias().parse_optional()
        return TypedefStatementSpecificItemAst(c1, p1, p2)

    @parser_rule
    def parse_typedef_statement_specific_items(self) -> TypedefStatementSpecificItemsAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkBraceL).parse_once()
        p2 = self.parse_typedef_statement_specific_item().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBraceR).parse_once()
        return TypedefStatementSpecificItemsAst(c1, p1, p2, p3)

    @parser_rule
    def parse_typedef_statement_all_items(self) -> TypedefStatementAllItemsAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkMul).parse_once()
        return TypedefStatementAllItemsAst(c1, p1)

    @parser_rule
    def parse_typedef_statement_specific_item_alias(self) -> TypedefStatementSpecificItemAliasAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkAs).parse_once()
        p2 = self.parse_upper_identifier().parse_once()
        return TypedefStatementSpecificItemAliasAst(c1, p1, p2)

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
        p8 = (p1 | p2 | p3 | p4 | p5 | p6).parse_once()
        return p8

    @parser_rule
    def parse_pattern_guard(self) -> PatternGuardAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkLogicalAnd).parse_once()
        p2 = self.parse_expression().parse_once()
        return PatternGuardAst(c1, p1, p2)

    @parser_rule
    def parse_pattern_variant(self) -> PatternVariantAst:
        p1 = self.parse_pattern_variant_tuple().for_alt()
        p2 = self.parse_pattern_variant_destructure().for_alt()
        p3 = self.parse_pattern_variant_variable().for_alt()
        p4 = self.parse_pattern_variant_literal().for_alt()
        p5 = self.parse_pattern_variant_bool_member().for_alt()
        p6 = self.parse_pattern_variant_else().for_alt()
        p7 = (p1 | p2 | p3 | p4 | p5 | p6).parse_once()
        return p7

    @parser_rule
    def parse_pattern_variant_tuple(self) -> PatternVariantTupleAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_pattern_variant().parse_zero_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return PatternVariantTupleAst(c1, p1, p2, p3)

    @parser_rule
    def parse_pattern_variant_destructure(self) -> PatternVariantDestructureAst:
        c1 = self.current_pos()
        p1 = self.parse_type_single().parse_once()
        p2 = self.parse_token(TokenType.TkBraceL).parse_once()
        p3 = self.parse_pattern_variant().parse_zero_or_more(TokenType.TkComma)
        p4 = self.parse_token(TokenType.TkBraceR).parse_once()
        return PatternVariantDestructureAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_pattern_variant_variable(self) -> PatternVariantVariableAst:
        c1 = self.current_pos()
        p1 = self.parse_local_variable_single().parse_once()
        return PatternVariantVariableAst(c1, p1)

    @parser_rule
    def parse_pattern_variant_literal(self) -> PatternVariantLiteralAst:
        c1 = self.current_pos()
        p1 = self.parse_literal().parse_once()
        return PatternVariantLiteralAst(c1, p1)

    @parser_rule
    def parse_pattern_variant_bool_member(self) -> PatternVariantBoolMemberAst:
        c1 = self.current_pos()
        p1 = self.parse_postfix_expression().parse_once()
        return PatternVariantBoolMemberAst(c1, p1)

    @parser_rule
    def parse_pattern_variant_else(self) -> PatternVariantElseAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwElse).parse_once()
        return PatternVariantElseAst(c1, p1)

    # ===== OPERATORS =====

    @parser_rule
    def parse_binary_op_precedence_level_1(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkCoalesce).parse_once()
        return p1

    @parser_rule
    def parse_binary_op_precedence_level_2(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkLogicalOr).parse_once()
        return p1

    @parser_rule
    def parse_binary_op_precedence_level_3(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkLogicalAnd).parse_once()
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
        p1 = self.parse_token(TokenType.TkBitShiftL).for_alt()
        p2 = self.parse_token(TokenType.TkBitShiftR).for_alt()
        p3 = self.parse_token(TokenType.TkBitRotateL).for_alt()
        p4 = self.parse_token(TokenType.TkBitRotateR).for_alt()
        p5 = (p1 | p2 | p3 | p4).parse_once()
        return p5

    @parser_rule
    def parse_binary_op_precedence_level_6(self) -> TokenAst:
        p1 = self.parse_token(TokenType.TkBitOr).for_alt()
        p2 = self.parse_token(TokenType.TkBitXor).for_alt()
        p3 = self.parse_token(TokenType.TkAdd).for_alt()
        p4 = self.parse_token(TokenType.TkSub).for_alt()
        p5 = (p1 | p2 | p3 | p4).parse_once()
        return p5

    @parser_rule
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
    def parse_unary_op(self) -> TokenAst:
        p1 = self.parse_token(TokenType.KwAsync).parse_once()
        return p1

    @parser_rule
    def parse_postfix_op(self) -> TokenAst:
        p1 = self.parse_postfix_op_function_call().for_alt()
        p2 = self.parse_postfix_op_member_access().for_alt()
        p3 = self.parse_postfix_op_early_return().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    def parse_postfix_op_function_call(self) -> PostfixExpressionOperatorFunctionCallAst:
        c1 = self.current_pos()
        p1 = self.parse_generic_arguments().parse_optional()
        p2 = self.parse_function_call_arguments().parse_once()
        p3 = self.parse_token(TokenType.TkVariadic).parse_optional()
        return PostfixExpressionOperatorFunctionCallAst(c1, p1, p2, p3)

    @parser_rule
    def parse_postfix_op_member_access(self) -> PostfixExpressionOperatorMemberAccessAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkDot).parse_once()
        p2 = self.parse_identifier().for_alt()
        p3 = self.parse_number_integer().for_alt()
        p4 = (p2 | p3).parse_once()
        return PostfixExpressionOperatorMemberAccessAst(c1, p1, p4)

    @parser_rule
    def parse_postfix_op_early_return(self) -> PostfixExpressionOperatorEarlyReturnAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkQst).parse_once()
        return PostfixExpressionOperatorEarlyReturnAst(c1, p1)

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
        p1 = self.parse_token(TokenType.TkBraceL).parse_once()
        p2 = self.parse_object_initializer_argument().parse_zero_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkBraceR).parse_once()
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
        p2 = self.parse_token(TokenType.KwElse)
        p3 = self.parse_token(TokenType.KwSup)
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    # ===== LAMBDAS =====

    @parser_rule
    def parse_lambda_prototype(self) -> LambdaPrototypeAst:
        c1 = self.current_pos()
        p1 = self.parse_annotation().parse_zero_or_more()
        p2 = self.parse_token(TokenType.KwFn).parse_once()
        p3 = self.parse_generic_parameters().parse_optional()
        p4 = self.parse_function_parameters().parse_once()
        p5 = self.parse_token(TokenType.TkArrowR).parse_once()
        p6 = self.parse_type().parse_once()
        p7 = self.parse_lambda_capture_block().parse_optional()
        p8 = self.parse_where_block().parse_optional()
        p9 = self.parse_inner_scope(self.parse_statement).parse_once()
        return LambdaPrototypeAst(c1, p1, p2, p3, p4, p5, p6, p7, p8, p9)

    @parser_rule
    def parse_lambda_capture_block(self) -> LambdaCaptureBlockAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.KwWith).parse_once()
        p2 = self.parse_token(TokenType.TkBrackL).parse_once()
        p3 = self.parse_lambda_capture_item().parse_one_or_more(TokenType.TkComma)
        p4 = self.parse_token(TokenType.TkBrackR).parse_once()
        return LambdaCaptureBlockAst(c1, p1, p2, p3, p4)

    @parser_rule
    def parse_lambda_capture_item(self) -> LambdaCaptureItemAst:
        p1 = self.parse_lambda_capture_item_named().for_alt()
        p2 = self.parse_lambda_capture_item_normal().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_lambda_capture_item_normal(self):
        c1 = self.current_pos()
        p1 = self.parse_convention().parse_once()
        p2 = self.parse_identifier().parse_once()
        return LambdaCaptureItemNormalAst(c1, p1, p2)

    @parser_rule
    def parse_lambda_capture_item_named(self):
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_once()
        p2 = self.parse_token(TokenType.TkAssign).parse_once()
        p3 = self.parse_convention().parse_once()
        p4 = self.parse_identifier().parse_once()
        return LambdaCaptureItemNamedAst(c1, p1, p2, p3, p4)

    # ===== TYPES =====

    @parser_rule
    def parse_type(self) -> TypeAst:
        p1 = self.parse_type_single().for_alt()
        p2 = self.parse_type_tuple().for_alt()
        p3 = self.parse_type_union().for_alt()
        p4 = (p1 | p2 | p3).parse_once()
        return p4

    @parser_rule
    def parse_type_single(self) -> TypeSingleAst:
        c1 = self.current_pos()
        p1 = self.parse_type_namespace().parse_optional() or []
        p2 = self.parse_type_part().one_or_more(TokenType.TkDot)
        return TypeSingleAst(c1, p1.items + p2)

    @parser_rule
    def parse_type_tuple(self) -> TypeTupleAst:
        c1 = self.current_pos()
        p1 = self.parse_token(TokenType.TkParenL).parse_once()
        p2 = self.parse_type().parse_one_or_more(TokenType.TkComma)
        p3 = self.parse_token(TokenType.TkParenR).parse_once()
        return TypeTupleAst(c1, p1, p2, p3)

    @parser_rule
    def parse_type_non_union(self) -> TypeAst:
        p1 = self.parse_type_single().for_alt()
        p2 = self.parse_type_tuple().for_alt()
        p3 = (p1 | p2).parse_once()
        return p3

    @parser_rule
    def parse_type_union(self) -> TypeUnionAst:
        c1 = self.current_pos()
        p1 = self.parse_type_non_union().parse_one_or_more(TokenType.TkBitOr)
        return TypeUnionAst(c1, p1)

    @parser_rule
    def parse_type_namespace(self) -> TypeNamespaceAst:
        c1 = self.current_pos()
        p1 = self.parse_identifier().parse_one_or_more(TokenType.TkDot)
        return TypeNamespaceAst(c1, p1)

    @parser_rule
    def parse_type_part(self) -> TypePartAst:
        c1 = self.current_pos()
        p1 = self.parse_generic_identifier().for_alt()
        p2 = self.parse_number_integer().for_alt()
        p3 = self.parse_self_type_keyword().for_alt()
        p4 = (p1 | p2 | p3).parse_once()

    @parser_rule
    def parse_self_type_keyword(self) -> GenericIdentifierAst:
        p1 = self.parse_token(TokenType.KwSelfType).parse_once()
        return GenericIdentifierAst(**p1.__dict__, generic_arguments=None, identifier=p1.token.token_metadata)

    # ===== PLACEHOLDERS (FOR NOW) =====

    @parser_rule
    def parse_statement_typedef(self) -> TypedefStatementAst:
        ...

    @parser_rule
    def parse_annotation(self) -> AnnotationAst:
        ...

    @parser_rule
    def parse_identifier(self) -> IdentifierAst:
        ...

    @parser_rule
    def parse_convention(self) -> ConventionAst:
        ...

    @parser_rule
    def parse_token(self, token_type: TokenType) -> TokenAst:
        ...

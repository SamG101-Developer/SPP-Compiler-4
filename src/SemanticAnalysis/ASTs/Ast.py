from __future__ import annotations
from typing import List, Optional

from dataclasses import dataclass


@dataclass
class Ast:
    pos: int


@dataclass
class AnnotationAst(Ast):
    at_token: TokenAst
    identifier: ModuleIdentifierAst
    generic_arguments: GenericArgumentGroupAst
    arguments: FunctionArgumentGroupAst


@dataclass
class AssignmentStatementAst(Ast):
    lhs: List[ExpressionAst]
    op: TokenAst
    rhs: ExpressionAst


@dataclass
class BinaryExpressionAst(Ast):
    lhs: ExpressionAst
    op: TokenAst
    rhs: ExpressionAst


@dataclass
class ClassAttributeAst(Ast):
    annotations: List[AnnotationAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    type_declaration: TypeAst


@dataclass
class ClassPrototypeAst(Ast):
    annotations: List[AnnotationAst]
    class_token: TokenAst
    identifier: IdentifierAst
    generic_parameters: GenericParameterGroupAst
    where_block: WhereBlockAst
    body: InnerScopeAst


@dataclass
class ConventionMovAst(Ast):
    ...


@dataclass
class ConventionRefAst(Ast):
    ampersand_token: TokenAst


@dataclass
class ConventionMutAst(Ast):
    ampersand_token: TokenAst
    mut_token: TokenAst


ConventionAst = (
        ConventionMovAst |
        ConventionRefAst |
        ConventionMutAst)


@dataclass
class FunctionArgumentNormalAst(Ast):
    value: ExpressionAst
    convention: Optional[ConventionAst]
    unpack_token: Optional[TokenAst]


@dataclass
class FunctionArgumentNamedAst(FunctionArgumentNormalAst):
    identifier: Optional[IdentifierAst]
    assignment_token: Optional[TokenAst]


FunctionArgumentAst = (
        FunctionArgumentNormalAst |
        FunctionArgumentNamedAst)


@dataclass
class FunctionArgumentGroupAst(Ast):
    paren_l_token: TokenAst
    arguments: List[FunctionArgumentAst]
    paren_r_token: TokenAst


@dataclass
class FunctionParameterSelfAst(Ast):
    is_mutable: Optional[TokenAst]
    identifier: IdentifierAst
    convention: Optional[ConventionAst]


@dataclass
class FunctionParameterRequiredAst(FunctionParameterSelfAst):
    colon_token: TokenAst
    type_declaration: TypeAst


@dataclass
class FunctionParameterOptionalAst(FunctionParameterRequiredAst):
    assignment_token: TokenAst
    default_value: ExpressionAst


@dataclass
class FunctionParameterVariadicAst(FunctionParameterRequiredAst):
    variadic_token: TokenAst


FunctionParameterAst = (
        FunctionParameterSelfAst |
        FunctionParameterRequiredAst |
        FunctionParameterOptionalAst |
        FunctionParameterVariadicAst)


@dataclass
class FunctionParameterGroupAst(Ast):
    paren_l_token: TokenAst
    parameters: List[FunctionParameterAst]
    paren_r_token: TokenAst


@dataclass
class FunctionPrototypeAst(Ast):
    @dataclass
    class ReturnTypeAst:
        colon_token: TokenAst
        type_declaration: TypeAst

    annotations: List[AnnotationAst]
    fun_token: TokenAst
    identifier: IdentifierAst
    generic_parameters: GenericParameterGroupAst
    parameters: FunctionParameterGroupAst
    return_type: ReturnTypeAst
    where_block: WhereBlockAst
    body: InnerScopeAst


@dataclass
class GenericArgumentNormalAst(Ast):
    type: TypeAst


@dataclass
class GenericArgumentNamedAst(GenericArgumentNormalAst):
    identifier: IdentifierAst
    assignment_token: TokenAst


GenericArgumentAst = (
        GenericArgumentNormalAst |
        GenericArgumentNamedAst)


@dataclass
class GenericArgumentGroupAst(Ast):
    bracket_l_token: TokenAst
    arguments: List[GenericArgumentAst]
    bracket_r_token: TokenAst


@dataclass
class GenericIdentifierAst(Ast):
    identifier: IdentifierAst
    generic_arguments: GenericArgumentGroupAst


@dataclass
class GenericParameterRequiredAst(Ast):
    identifier: TypeAst
    inline_constraints: List[GenericParameterInlineConstraintAst]


@dataclass
class GenericParameterOptionalAst(GenericParameterRequiredAst):
    assignment_token: TokenAst
    default_value: TypeAst


@dataclass
class GenericParameterVariadicAst(GenericParameterRequiredAst):
    variadic_token: TokenAst


GenericParameterAst = (
        GenericParameterRequiredAst |
        GenericParameterOptionalAst |
        GenericParameterVariadicAst)


@dataclass
class GenericParameterGroupAst(Ast):
    bracket_l_token: TokenAst
    parameters: List[GenericParameterAst]
    bracket_r_token: TokenAst


@dataclass
class GenericParameterInlineConstraintAst(Ast):
    colon_token: TokenAst
    constraints: List[TypeAst]


@dataclass
class IdentifierAst(Ast):
    identifier: str


@dataclass
class IfExpressionAst(Ast):
    if_keyword: TokenAst
    condition: ExpressionAst
    comp_operator: TokenAst
    branches: List[PatternBlockAst]


@dataclass
class InnerScopeAst(Ast):
    brace_l_token: TokenAst
    members: List[ModuleMemberAst]
    brace_r_token: TokenAst


@dataclass
class LambdaCaptureBlockAst(Ast):
    with_keyword: TokenAst
    bracket_l_token: TokenAst
    items: List[LambdaCaptureItemAst]
    bracket_r_token: TokenAst


@dataclass
class LambdaCaptureItemAst:
    convention: Optional[ConventionAst]
    identifier: IdentifierAst


@dataclass
class LambdaPrototypeAst(FunctionPrototypeAst):
    ...


@dataclass
class LetStatementInitializedAst(Ast):
    let_keyword: TokenAst
    assign_to: LocalVariableAst
    assign_token: TokenAst
    value: ExpressionAst
    residual: Optional[ResidualAst]


@dataclass
class LetStatementUninitializedAst(Ast):
    let_keyword: TokenAst
    assign_to: LocalVariableAst
    colon_token: TokenAst
    type_declaration: TypeAst


LetStatementAst = (
        LetStatementInitializedAst |
        LetStatementUninitializedAst)


@dataclass
class LiteralNumberBase10Ast(Ast):
    sign: Optional[TokenAst]
    integer: TokenAst
    decimal: Optional[TokenAst]
    primitive_type: Optional[IdentifierAst]  # TypeAst?


@dataclass
class LiteralNumberBase2Ast(Ast):
    integer: TokenAst
    primitive_type: Optional[IdentifierAst]


@dataclass
class LiteralNumberBase16Ast(Ast):
    integer: TokenAst
    primitive_type: Optional[IdentifierAst]


LiteralNumberAst = (
        LiteralNumberBase10Ast |
        LiteralNumberBase2Ast |
        LiteralNumberBase16Ast)


@dataclass
class LiteralStringAst(Ast):
    string: TokenAst


@dataclass
class LiteralArrayNonEmptyAst(Ast):
    bracket_l_token: TokenAst
    items: List[ExpressionAst]
    bracket_r_token: TokenAst


@dataclass
class LiteralArrayEmptyAst(Ast):
    bracket_l_token: TokenAst
    element_type: TypeAst
    bracket_r_token: TokenAst


LiteralArrayAst = (
        LiteralArrayNonEmptyAst |
        LiteralArrayEmptyAst)


@dataclass
class LiteralTupleAst(Ast):
    paren_l_token: TokenAst
    items: List[ExpressionAst]
    paren_r_token: TokenAst


@dataclass
class LiteralBooleanAst(Ast):
    boolean: TokenAst


@dataclass
class LiteralRegexAst(Ast):
    regex: TokenAst


LiteralAst = (
        LiteralNumberAst |
        LiteralStringAst |
        LiteralArrayAst |
        LiteralTupleAst |
        LiteralBooleanAst |
        LiteralRegexAst)


@dataclass
class LocalVariableAst:
    ...


@dataclass
class ModuleIdentifierAst:
    ...


@dataclass
class ModuleMemberAst:
    ...


@dataclass
class ModulePrototypeAst:
    ...


@dataclass
class ObjectInitializerArgumentAst:
    ...


@dataclass
class ObjectInitializerArgumentGroupAst:
    ...


@dataclass
class ObjectInitializerAst:
    ...


@dataclass
class ParenthesizedExpressionAst:
    ...


@dataclass
class PatternAst:
    ...


@dataclass
class PatternBlockAst:
    ...


@dataclass
class PatternGuardAst:
    ...


@dataclass
class PostfixExpressionAst:
    ...


@dataclass
class PostfixExpressionVariantAst:
    ...


@dataclass
class PostfixMemberPartAst:
    ...


@dataclass
class PrimaryExpressionAst:
    ...


@dataclass
class ProgramAst:
    ...


@dataclass
class ResidualAst:
    ...


@dataclass
class ReturnStatementAst:
    ...


@dataclass
class StatementAst:
    ...


@dataclass
class SupMemberAst:
    ...


@dataclass
class SupMethodPrototypeAst:
    ...


@dataclass
class SupPrototypeInheritanceAst(SupPrototypeNormalAst):
    ...


@dataclass
class SupPrototypeNormalAst:
    ...


@dataclass
class SupTypedefAst:
    ...


@dataclass
class TokenAst:
    ...


@dataclass
class TypeAst:
    ...


@dataclass
class TypePartAst:
    ...


@dataclass
class TypeReductionStatementAst:
    ...


@dataclass
class TypedefStatementAst:
    ...


@dataclass
class UnaryExpressionAst:
    ...


@dataclass
class UnaryExpressionVariantAst:
    ...


@dataclass
class WhereBlockAst:
    ...


@dataclass
class WhereConstraintsAst:
    ...


@dataclass
class WhileExpressionAst:
    ...


@dataclass
class WithExpressionAliasAst:
    ...


@dataclass
class WithExpressionAst:
    ...


@dataclass
class YieldStatementAst:
    ...


ExpressionAst = (
        BinaryExpressionAst |
        UnaryExpressionAst |
        PostfixExpressionAst |
        PrimaryExpressionAst |
        TokenAst)

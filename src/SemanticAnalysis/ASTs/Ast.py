from __future__ import annotations
from dataclasses import dataclass
from typing import List, Optional

from src.LexicalAnalysis.Tokens import Token


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
    body: InnerScopeAst[ClassAttributeAst]


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
    convention: Optional[ConventionAst]
    unpack_token: Optional[TokenAst]
    value: ExpressionAst


@dataclass
class FunctionArgumentNamedAst(Ast):
    identifier: Optional[IdentifierAst]
    assignment_token: Optional[TokenAst]
    convention: Optional[ConventionAst]
    value: ExpressionAst


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
    convention: Optional[ConventionAst]
    identifier: TokenAst


@dataclass
class FunctionParameterRequiredAst(Ast):
    is_mutable: Optional[TokenAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: Optional[ConventionAst]
    type_declaration: TypeAst


@dataclass
class FunctionParameterOptionalAst(Ast):
    is_mutable: Optional[TokenAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: Optional[ConventionAst]
    type_declaration: TypeAst
    assignment_token: TokenAst
    default_value: ExpressionAst


@dataclass
class FunctionParameterVariadicAst(Ast):
    is_mutable: Optional[TokenAst]
    variadic_token: TokenAst
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: Optional[ConventionAst]
    type_declaration: TypeAst


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
    annotations: List[AnnotationAst]
    fun_token: TokenAst
    identifier: IdentifierAst
    generic_parameters: GenericParameterGroupAst
    parameters: FunctionParameterGroupAst
    arrow_token: TokenAst
    return_type: TypeAst
    where_block: WhereBlockAst
    lambda_capture_block: Optional[LambdaCaptureBlockAst]
    body: InnerScopeAst[StatementAst]


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
class InnerScopeAst[T](Ast):
    brace_l_token: TokenAst
    members: List[T]
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
class LocalVariableSingleAst(Ast):
    is_mutable: Optional[TokenAst]
    unpack_token: Optional[TokenAst]
    identifier: IdentifierAst


@dataclass
class LocalVariableMultipleAst(Ast):
    paren_l_token: TokenAst
    items: List[LocalVariableSingleAst]
    paren_r_token: TokenAst


@dataclass
class LocalVariableDestructuredAst(Ast):
    class_type: TypeAst
    bracket_l_token: TokenAst
    items: List[LocalVariableSingleAst]
    bracket_r_token: TokenAst


LocalVariableAst = (
        LocalVariableSingleAst |
        LocalVariableMultipleAst |
        LocalVariableDestructuredAst)


@dataclass
class ModuleIdentifierAst(Ast):
    parts: List[IdentifierAst]


@dataclass
class ModulePrototypeAst(Ast):
    annotations: List[AnnotationAst]
    module_keyword: TokenAst
    identifier: ModuleIdentifierAst
    body: List[ModuleMemberAst]


@dataclass
class ObjectInitializerArgumentNormalAst(Ast):
    identifier: IdentifierAst


@dataclass
class ObjectInitializerArgumentNamedAst(Ast):
    identifier: IdentifierAst
    assignment_token: TokenAst
    value: ExpressionAst


ObjectInitializerArgumentAst = (
        ObjectInitializerArgumentNormalAst |
        ObjectInitializerArgumentNamedAst)


@dataclass
class ObjectInitializerArgumentGroupAst(Ast):
    brace_l_token: TokenAst
    arguments: List[ObjectInitializerArgumentAst]
    brace_r_token: TokenAst


@dataclass
class ObjectInitializerAst(Ast):
    class_type: TypeAst
    arguments: ObjectInitializerArgumentGroupAst


@dataclass
class ParenthesizedExpressionAst(Ast):
    paren_l_token: TokenAst
    expression: ExpressionAst
    paren_r_token: TokenAst


@dataclass
class PatternTypeTupleAst(Ast):
    paren_l_token: TokenAst
    items: List[PatternAst]
    paren_r_token: TokenAst


@dataclass
class PatternTypeDestructureAst(Ast):
    class_type: TypeAst
    bracket_l_token: TokenAst
    items: List[PatternAst]
    bracket_r_token: TokenAst


@dataclass
class PatternTypeVariableAst(Ast):
    variable: LocalVariableSingleAst


@dataclass
class PatternTypeLiteralAst(Ast):
    literal: LiteralAst


@dataclass
class PatternTypeBoolMember(Ast):
    expression: PostfixExpressionAst


@dataclass
class PatternTypeElseAst(Ast):
    else_token: TokenAst


PatternAst = (
        PatternTypeTupleAst |
        PatternTypeDestructureAst |
        PatternTypeVariableAst |
        PatternTypeLiteralAst |
        PatternTypeBoolMember |
        PatternTypeElseAst)


@dataclass
class PatternBlockAst(Ast):
    comp_operator: TokenAst
    patterns: List[PatternAst]
    guard: Optional[PatternGuardAst]
    body: InnerScopeAst[StatementAst]


@dataclass
class PatternGuardAst(Ast):
    guard_token: TokenAst
    expression: ExpressionAst


@dataclass
class PostfixExpressionAst(Ast):
    lhs: ExpressionAst
    op: PostfixExpressionOperatorAst


@dataclass
class PostfixExpressionOperatorFunctionCallAst(Ast):
    generic_arguments: GenericArgumentGroupAst
    arguments: FunctionArgumentGroupAst
    fold_token: Optional[TokenAst]


@dataclass
class PostfixExpressionOperatorMemberAccessAst(Ast):
    dot_token: TokenAst
    identifier: PostfixMemberPartAst


@dataclass
class PostfixExpressionOperatorEarlyReturnAst(Ast):
    return_token: TokenAst


PostfixExpressionOperatorAst = (
        PostfixExpressionOperatorFunctionCallAst |
        PostfixExpressionOperatorMemberAccessAst |
        PostfixExpressionOperatorEarlyReturnAst)

PostfixMemberPartAst = (
        IdentifierAst |
        LiteralNumberBase10Ast)


@dataclass
class ProgramAst(Ast):
    module: ModulePrototypeAst
    eof_token: TokenAst


@dataclass
class ResidualAst(Ast):
    else_keyword: TokenAst
    body: InnerScopeAst[StatementAst]


@dataclass
class ReturnStatementAst(Ast):
    return_keyword: TokenAst
    expression: Optional[ExpressionAst]


@dataclass
class SupMethodPrototypeAst(FunctionPrototypeAst):
    ...


@dataclass
class SupPrototypeNormalAst(Ast):
    sup_keyword: TokenAst
    generic_parameters: GenericParameterGroupAst
    identifier: IdentifierAst
    where_block: WhereBlockAst
    body: InnerScopeAst[SupMemberAst]


@dataclass
class SupPrototypeInheritanceAst(Ast):
    sup_keyword: TokenAst
    generic_parameters: GenericParameterGroupAst
    super_class: TypeAst
    on_keyword: TokenAst
    identifier: TypeAst
    where_block: WhereBlockAst
    body: InnerScopeAst[SupMemberAst]


@dataclass
class TypedefStatementAst(Ast):
    typedef_keyword: TokenAst
    generic_parameters: GenericParameterGroupAst
    old_type: TypeAst
    where_block: WhereBlockAst
    as_keyword: TokenAst
    new_type: TypeAst


@dataclass
class SupTypedefAst(TypedefStatementAst):
    annotations: List[AnnotationAst]


@dataclass
class TokenAst(Ast):
    token: Token


@dataclass
class TypeNormalAst(Ast):
    parts: List[TypePartAst]


@dataclass
class TypeTupleAst(Ast):
    paren_l_token: TokenAst
    items: List[TypeAst]
    paren_r_token: TokenAst


@dataclass
class TypeUnionAst(Ast):
    items: List[TypeAst]


TypeAst = (
        TypeNormalAst |
        TypeTupleAst |
        TypeUnionAst)

TypePartAst = (
        IdentifierAst |
        GenericIdentifierAst |
        LiteralNumberBase10Ast)


@dataclass
class UnaryExpressionAst(Ast):
    op: UnaryOperatorAst
    rhs: ExpressionAst


UnaryOperatorAst = TokenAst


@dataclass
class WhereBlockAst(Ast):
    where_keyword: TokenAst
    constraint_group: WhereConstraintsGroupAst


@dataclass
class WhereConstraintsGroupAst(Ast):
    paren_l_token: TokenAst
    constraints: List[WhereConstraintsAst]
    paren_r_token: TokenAst


@dataclass
class WhereConstraintsAst(Ast):
    types_to_constrain: List[TypeAst]
    colon_token: TokenAst
    constraints: List[TypeAst]


@dataclass
class WhileExpressionAst(Ast):
    while_keyword: TokenAst
    condition: ExpressionAst
    body: InnerScopeAst[StatementAst]
    else_block: Optional[ResidualAst]


@dataclass
class WithExpressionAliasAst(Ast):
    variable: LocalVariableAst
    assign_token: TokenAst


@dataclass
class WithExpressionAst(Ast):
    with_keyword: TokenAst
    alias: Optional[WithExpressionAliasAst]
    expression: ExpressionAst
    body: InnerScopeAst[StatementAst]


@dataclass
class YieldStatementAst(Ast):
    yield_keyword: TokenAst
    convention: Optional[ConventionAst]
    expression: ExpressionAst


PrimaryExpressionAst = (
        LiteralAst |
        IdentifierAst |
        LambdaPrototypeAst |
        ParenthesizedExpressionAst |
        ObjectInitializerAst |
        TypeAst |
        IfExpressionAst |
        WhileExpressionAst |
        WithExpressionAst |
        YieldStatementAst |
        InnerScopeAst |  # [StatementAst]
        TokenAst)

ExpressionAst = (
        BinaryExpressionAst |
        UnaryExpressionAst |
        PostfixExpressionAst |
        PrimaryExpressionAst |
        TokenAst)

StatementAst = (
        TypedefStatementAst |
        ReturnStatementAst |
        AssignmentStatementAst |
        LetStatementAst |
        ExpressionAst)

SupMemberAst = (
        SupMethodPrototypeAst |
        SupTypedefAst |
        ClassPrototypeAst |
        LetStatementAst |
        SupPrototypeInheritanceAst)

ModuleMemberAst = (
        ClassPrototypeAst |
        FunctionPrototypeAst |
        TypedefStatementAst |
        SupPrototypeNormalAst |
        SupPrototypeInheritanceAst |
        LetStatementAst)

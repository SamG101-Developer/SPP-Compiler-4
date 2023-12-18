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
class IfExpressionAst:
    ...


@dataclass
class InnerScopeAst:
    ...


@dataclass
class LambdaCaptureBlockAst:
    ...


@dataclass
class LambdaCaptureItemAst:
    ...


@dataclass
class LambdaPrototypeAst:
    ...


@dataclass
class LetStatementAst:
    ...


@dataclass
class LiteralAst:
    ...


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
        BinaryExpressionAst  |
        UnaryExpressionAst   |
        PostfixExpressionAst |
        PrimaryExpressionAst |
        TokenAst)

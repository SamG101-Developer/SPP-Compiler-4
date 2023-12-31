from __future__ import annotations

import dataclasses
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Optional, Self

from src.SemanticAnalysis.PreProcessor import PreProcessor
from src.SemanticAnalysis.CommonTypes import CommonTypes
from src.LexicalAnalysis.Tokens import Token, TokenType
from src.Utils.Sequence import Seq


class Default(ABC):
    @staticmethod
    @abstractmethod
    def default() -> Self:
        ...


@dataclass
class Ast(ABC):
    pos: int
    
    @abstractmethod
    def print(self) -> str:
        ...


@dataclass
class AnnotationAst(Ast):
    at_token: TokenAst
    identifier: ModuleIdentifierAst
    generic_arguments: GenericArgumentGroupAst
    arguments: FunctionArgumentGroupAst

    def print(self) -> str:
        return f"{self.at_token.print()}{self.identifier.print()}{self.generic_arguments.print()}{self.arguments.print()}"


@dataclass
class AssignmentStatementAst(Ast):
    lhs: List[ExpressionAst]
    op: TokenAst
    rhs: ExpressionAst

    def print(self) -> str:
        return f"{Seq(self.lhs).print(", ")} {self.op.print()} {self.rhs.print()}"


@dataclass
class BinaryExpressionAst(Ast):
    lhs: ExpressionAst
    op: TokenAst
    rhs: ExpressionAst

    def print(self) -> str:
        return f"{self.lhs.print()} {self.op.print()} {self.rhs.print()}"


@dataclass
class ClassAttributeAst(Ast):
    annotations: List[AnnotationAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    type_declaration: TypeAst

    def print(self) -> str:
        return f"{Seq(self.annotations).print("\n")}\n{self.identifier.print()}{self.colon_token.print()} {self.type_declaration.print()}"


@dataclass
class ClassPrototypeAst(Ast, PreProcessor):
    annotations: List[AnnotationAst]
    class_token: TokenAst
    identifier: TypeAst
    generic_parameters: Optional[GenericParameterGroupAst]
    where_block: Optional[WhereBlockAst]
    body: InnerScopeAst[ClassAttributeAst]
    _mod: ModuleIdentifierAst = dataclasses.field(default=None)

    def print(self) -> str:
        s = ""
        s += f"{Seq(self.annotations).print("\n")}\n{self.class_token.print()}{self.identifier.print()}"
        s += f"{self.generic_parameters.print()}" if self.generic_parameters else ""
        s += f"{self.where_block.print()}" if self.where_block else ""
        s += f"{self.body.print()}"
        return s

    def pre_process(self, context: ModulePrototypeAst) -> None:
        Seq(self.body.members).for_each(lambda m: m.type_declaration.substitute_generics(CommonTypes.self(), self.identifier))
        Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), self.identifier))
        self._mod = context.identifier


@dataclass
class ConventionMovAst(Ast):
    def print(self) -> str:
        return f""


@dataclass
class ConventionRefAst(Ast):
    ampersand_token: TokenAst

    def print(self) -> str:
        return f"{self.ampersand_token.print()}"


@dataclass
class ConventionMutAst(Ast):
    ampersand_token: TokenAst
    mut_token: TokenAst

    def print(self) -> str:
        return f"{self.ampersand_token.print()}{self.mut_token.print()}"


ConventionAst = (
        ConventionMovAst |
        ConventionRefAst |
        ConventionMutAst)


@dataclass
class FunctionArgumentNormalAst(Ast):
    convention: ConventionAst
    unpack_token: Optional[TokenAst]
    value: ExpressionAst

    def print(self) -> str:
        s = ""
        s += f"{self.convention.print()}"
        s += f"{self.unpack_token.print()}" if self.unpack_token else ""
        s += f"{self.value.print()}"
        return s


@dataclass
class FunctionArgumentNamedAst(Ast):
    identifier: IdentifierAst
    assignment_token: TokenAst
    convention: ConventionAst
    value: ExpressionAst

    def print(self) -> str:
        return f"{self.identifier.print()}{self.assignment_token.print()}{self.convention.print()}{self.value.print()}"


FunctionArgumentAst = (
        FunctionArgumentNormalAst |
        FunctionArgumentNamedAst)


@dataclass
class FunctionArgumentGroupAst(Ast):
    paren_l_token: TokenAst
    arguments: List[FunctionArgumentAst]
    paren_r_token: TokenAst

    def print(self) -> str:
        return f"{self.paren_l_token.print()}{Seq(self.arguments).print(", ")}{self.paren_r_token.print()}"


@dataclass
class FunctionParameterSelfAst(Ast):
    is_mutable: Optional[TokenAst]
    convention: ConventionAst
    identifier: TokenAst

    def print(self) -> str:
        s = ""
        s += f"{self.is_mutable.print()}" if self.is_mutable else ""
        s += f"{self.convention.print()} {self.identifier.print()}"
        return s


@dataclass
class FunctionParameterRequiredAst(Ast):
    is_mutable: Optional[TokenAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: ConventionAst
    type_declaration: TypeAst

    def print(self) -> str:
        s = ""
        s += f"{self.is_mutable.print()}" if self.is_mutable else ""
        s += f"{self.identifier.print()}{self.colon_token.print()} {self.convention.print()}{self.type_declaration.print()}"
        return s


@dataclass
class FunctionParameterOptionalAst(Ast):
    is_mutable: Optional[TokenAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: ConventionAst
    type_declaration: TypeAst
    assignment_token: TokenAst
    default_value: ExpressionAst

    def print(self) -> str:
        s = ""
        s += f"{self.is_mutable.print()}" if self.is_mutable else ""
        s += f"{self.identifier.print()}{self.colon_token.print()} {self.convention.print()}{self.type_declaration.print()} {self.assignment_token.print()} {self.default_value.print()}"
        return s


@dataclass
class FunctionParameterVariadicAst(Ast):
    is_mutable: Optional[TokenAst]
    variadic_token: TokenAst
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: ConventionAst
    type_declaration: TypeAst

    def print(self) -> str:
        s = ""
        s += f"{self.is_mutable.print()}" if self.is_mutable else ""
        s += f"{self.variadic_token.print()}{self.identifier.print()}{self.colon_token.print()} {self.convention.print()}{self.type_declaration.print()}"
        return s


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

    def print(self) -> str:
        return f"{self.paren_l_token.print()}{Seq(self.parameters).print(", ")}{self.paren_r_token.print()}"


@dataclass
class FunctionPrototypeAst(Ast, PreProcessor):
    annotations: List[AnnotationAst]
    fun_token: TokenAst
    identifier: IdentifierAst
    generic_parameters: GenericParameterGroupAst
    parameters: FunctionParameterGroupAst
    arrow_token: TokenAst
    return_type: TypeAst
    where_block: Optional[WhereBlockAst]
    body: InnerScopeAst[StatementAst]

    def print(self) -> str:
        s = ""
        s += f"{Seq(self.annotations).print("\n")}\n{self.fun_token.print()}{self.identifier.print()}{self.generic_parameters.print()}{self.parameters.print()} {self.arrow_token.print()} {self.return_type.print()}"
        s += f"{self.where_block.print()}" if self.where_block else ""
        s += f"{self.body.print()}"
        return s

    def pre_process(self, context: ModulePrototypeAst | SupPrototypeAst) -> None:
        ...


@dataclass
class GenericArgumentNormalAst(Ast):
    type: TypeAst

    def print(self) -> str:
        return f"{self.type.print()}"


@dataclass
class GenericArgumentNamedAst(GenericArgumentNormalAst):
    identifier: IdentifierAst
    assignment_token: TokenAst

    def print(self) -> str:
        return f"{self.identifier.print()}{self.assignment_token.print()}{self.type.print()}"


GenericArgumentAst = (
        GenericArgumentNormalAst |
        GenericArgumentNamedAst)


@dataclass
class GenericArgumentGroupAst(Ast, Default):
    bracket_l_token: TokenAst
    arguments: List[GenericArgumentAst]
    bracket_r_token: TokenAst

    @staticmethod
    def default() -> Self:
        return GenericArgumentGroupAst(-1, TokenAst.no_tok(), [], TokenAst.no_tok())

    def print(self) -> str:
        return f"{self.bracket_l_token.print()}{Seq(self.arguments).print(", ")}{self.bracket_r_token.print()}"


@dataclass
class GenericIdentifierAst(Ast, Default):
    identifier: str
    generic_arguments: Optional[GenericArgumentGroupAst]

    @staticmethod
    def default() -> Self:
        return GenericIdentifierAst(-1, "", GenericArgumentGroupAst.default())

    def print(self) -> str:
        s = ""
        s += f"{self.identifier}"
        s += f"{self.generic_arguments.print()}" if self.generic_arguments else ""
        return s


@dataclass
class GenericParameterRequiredAst(Ast):
    identifier: TypeAst
    inline_constraints: Optional[GenericParameterInlineConstraintAst]

    def print(self) -> str:
        s = ""
        s += f"{self.identifier.print()}"
        s += f"{self.inline_constraints.print()}" if self.inline_constraints else ""
        return s


@dataclass
class GenericParameterOptionalAst(GenericParameterRequiredAst):
    assignment_token: TokenAst
    default_value: TypeAst

    def print(self) -> str:
        return f"{super().print()} {self.assignment_token.print()} {self.default_value.print()}"


@dataclass
class GenericParameterVariadicAst(GenericParameterRequiredAst):
    variadic_token: TokenAst

    def print(self) -> str:
        return f"{self.variadic_token.print()}{super().print()}"


GenericParameterAst = (
        GenericParameterRequiredAst |
        GenericParameterOptionalAst |
        GenericParameterVariadicAst)


@dataclass
class GenericParameterGroupAst(Ast, Default):
    bracket_l_token: TokenAst
    parameters: List[GenericParameterAst]
    bracket_r_token: TokenAst

    @staticmethod
    def default() -> Self:
        return GenericParameterGroupAst(-1, None, [], None)

    def print(self) -> str:
        return f"{self.bracket_l_token.print()}{Seq(self.parameters).print(", ")}{self.bracket_r_token.print()}"

    def get_req(self) -> List[GenericParameterRequiredAst]:
        return [p for p in self.parameters if isinstance(p, GenericParameterRequiredAst)]

    def get_opt(self) -> List[GenericParameterOptionalAst]:
        return [p for p in self.parameters if isinstance(p, GenericParameterOptionalAst)]

    def get_var(self) -> List[GenericParameterVariadicAst]:
        return [p for p in self.parameters if isinstance(p, GenericParameterVariadicAst)]


@dataclass
class GenericParameterInlineConstraintAst(Ast, Default):
    colon_token: TokenAst
    constraints: List[TypeAst]

    @staticmethod
    def default() -> Self:
        return GenericParameterInlineConstraintAst(-1, TokenAst.no_tok(), [])

    def print(self) -> str:
        return f"{self.colon_token.print()} {Seq(self.constraints).print(", ")}"


@dataclass
class IdentifierAst(Ast):
    value: str

    def print(self) -> str:
        return f"{self.value}"


@dataclass
class IfExpressionAst(Ast):
    if_keyword: TokenAst
    condition: ExpressionAst
    comp_operator: TokenAst
    branches: List[PatternBlockAst]

    def print(self) -> str:
        return f"{self.if_keyword.print()}{self.condition.print()} {self.comp_operator.print()} {Seq(self.branches).print("\n")}"


@dataclass
class InnerScopeAst[T](Ast, Default):
    brace_l_token: TokenAst
    members: List[T]
    brace_r_token: TokenAst

    @staticmethod
    def default() -> Self:
        return InnerScopeAst(-1, TokenAst.no_tok(), [], TokenAst.no_tok())

    def print(self) -> str:
        return f"{self.brace_l_token.print()}\n{Seq(self.members).print("\n")}\n{self.brace_r_token.print()}"


@dataclass
class LambdaCaptureBlockAst(Ast, Default):
    with_keyword: TokenAst
    bracket_l_token: TokenAst
    items: List[LambdaCaptureItemAst]
    bracket_r_token: TokenAst

    @staticmethod
    def default() -> Self:
        return LambdaCaptureBlockAst(-1, TokenAst.no_tok(), TokenAst.no_tok(), [], TokenAst.no_tok())

    def print(self) -> str:
        return f"{self.with_keyword.print()}{self.bracket_l_token.print()}{Seq(self.items).print(", ")}{self.bracket_r_token.print()}"


@dataclass
class LambdaCaptureItemNormalAst(Ast):
    convention: ConventionAst
    value: IdentifierAst

    def print(self) -> str:
        return f"{self.convention.print()}{self.value.print()}"


@dataclass
class LambdaCaptureItemNamedAst(Ast):
    identifier: IdentifierAst
    assignment_token: TokenAst
    convention: ConventionAst
    value: IdentifierAst

    def print(self) -> str:
        return f"{self.identifier.print()}{self.assignment_token.print()}{self.convention.print()}{self.value.print()}"


LambdaCaptureItemAst = (
        LambdaCaptureItemNormalAst |
        LambdaCaptureItemNamedAst)


@dataclass
class LambdaPrototypeAst(Ast):
    annotations: List[AnnotationAst]
    fun_token: TokenAst
    generic_parameters: GenericParameterGroupAst
    parameters: FunctionParameterGroupAst
    arrow_token: TokenAst
    return_type: TypeAst
    where_block: Optional[WhereBlockAst]
    lambda_capture_block: Optional[LambdaCaptureBlockAst]
    body: InnerScopeAst[StatementAst]

    def print(self) -> str:
        s = ""
        s += f"{Seq(self.annotations).print("\n")}\n{self.fun_token.print()}{self.generic_parameters.print()}{self.parameters.print()} {self.arrow_token.print()} {self.return_type.print()}"
        s += f"{self.where_block.print()}" if self.where_block else ""
        s += f"{self.lambda_capture_block.print()}" if self.lambda_capture_block else ""
        s += f"{self.body.print()}"
        return s


@dataclass
class LetStatementInitializedAst(Ast):
    let_keyword: TokenAst
    assign_to: LocalVariableAst
    assign_token: TokenAst
    value: ExpressionAst
    residual: Optional[ResidualInnerScopeAst]

    def print(self) -> str:
        s = ""
        s += f"{self.let_keyword.print()}{self.assign_to.print()} {self.assign_token.print()} {self.value.print()}"
        s += f"{self.residual.print()}" if self.residual else ""
        return s


@dataclass
class LetStatementUninitializedAst(Ast):
    let_keyword: TokenAst
    assign_to: LocalVariableAst
    colon_token: TokenAst
    type_declaration: TypeAst

    def print(self) -> str:
        return f"{self.let_keyword.print()}{self.assign_to.print()}{self.colon_token.print()} {self.type_declaration.print()}"


LetStatementAst = (
        LetStatementInitializedAst |
        LetStatementUninitializedAst)


@dataclass
class LiteralNumberBase10Ast(Ast):
    sign: Optional[TokenAst]
    number: TokenAst
    primitive_type: Optional[IdentifierAst]  # TypeAst?

    def print(self) -> str:
        s = ""
        s += f"{self.sign.print()}" if self.sign else ""
        s += f"{self.number.print()}"
        s += f"{self.primitive_type.print()}" if self.primitive_type else ""
        return s


@dataclass
class LiteralNumberBase02Ast(Ast):
    integer: TokenAst
    primitive_type: Optional[IdentifierAst]

    def print(self) -> str:
        s = ""
        s += f"{self.integer.print()}"
        s += f"{self.primitive_type.print()}" if self.primitive_type else ""
        return s


@dataclass
class LiteralNumberBase16Ast(Ast):
    integer: TokenAst
    primitive_type: Optional[IdentifierAst]

    def print(self) -> str:
        s = ""
        s += f"{self.integer.print()}"
        s += f"{self.primitive_type.print()}" if self.primitive_type else ""
        return s


LiteralNumberAst = (
        LiteralNumberBase10Ast |
        LiteralNumberBase02Ast |
        LiteralNumberBase16Ast)


@dataclass
class LiteralStringAst(Ast):
    string: TokenAst

    def print(self) -> str:
        return f"{self.string.print()}"


@dataclass
class LiteralArrayNonEmptyAst(Ast):
    bracket_l_token: TokenAst
    items: List[ExpressionAst]
    bracket_r_token: TokenAst

    def print(self) -> str:
        return f"{self.bracket_l_token.print()}{Seq(self.items).print(", ")}{self.bracket_r_token.print()}"


@dataclass
class LiteralArrayEmptyAst(Ast):
    bracket_l_token: TokenAst
    element_type: TypeAst
    bracket_r_token: TokenAst

    def print(self) -> str:
        return f"{self.bracket_l_token.print()}{self.element_type.print()}{self.bracket_r_token.print()}"


LiteralArrayAst = (
        LiteralArrayNonEmptyAst |
        LiteralArrayEmptyAst)


@dataclass
class LiteralTupleAst(Ast):
    paren_l_token: TokenAst
    items: List[ExpressionAst]
    paren_r_token: TokenAst

    def print(self) -> str:
        return f"{self.paren_l_token.print()}{Seq(self.items).print(", ")}{self.paren_r_token.print()}"


@dataclass
class LiteralBooleanAst(Ast):
    boolean: TokenAst

    def print(self) -> str:
        return f"{self.boolean.print()}"


@dataclass
class LiteralRegexAst(Ast):
    regex: TokenAst

    def print(self) -> str:
        return f"{self.regex.print()}"


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

    def print(self) -> str:
        s = ""
        s += f"{self.is_mutable.print()}" if self.is_mutable else ""
        s += f"{self.unpack_token.print()}" if self.unpack_token else ""
        s += f"{self.identifier.print()}"
        return s


@dataclass
class LocalVariableTupleAst(Ast):
    paren_l_token: TokenAst
    items: List[LocalVariableSingleAst]
    paren_r_token: TokenAst

    def print(self) -> str:
        return f"{self.paren_l_token.print()}{Seq(self.items).print(", ")}{self.paren_r_token.print()}"


@dataclass
class LocalVariableDestructureAst(Ast):
    class_type: TypeAst
    bracket_l_token: TokenAst
    items: List[LocalVariableSingleAst]
    bracket_r_token: TokenAst

    def print(self) -> str:
        return f"{self.class_type.print()}{self.bracket_l_token.print()}{Seq(self.items).print(", ")}{self.bracket_r_token.print()}"


LocalVariableAst = (
        LocalVariableSingleAst |
        LocalVariableTupleAst |
        LocalVariableDestructureAst)


@dataclass
class ModuleIdentifierAst(Ast):
    parts: List[IdentifierAst]

    def print(self) -> str:
        return f"{Seq(self.parts).print(".")}"


@dataclass
class ModulePrototypeAst(Ast):
    annotations: List[AnnotationAst]
    module_keyword: TokenAst
    identifier: ModuleIdentifierAst
    body: List[ModuleMemberAst]

    def print(self) -> str:
        return f"{Seq(self.annotations).print("\n")}\n{self.module_keyword.print()}{self.identifier.print()}\n{Seq(self.body).print("\n")}"


@dataclass
class ObjectInitializerArgumentNormalAst(Ast):
    identifier: IdentifierAst

    def print(self) -> str:
        return f"{self.identifier.print()}"


@dataclass
class ObjectInitializerArgumentNamedAst(Ast):
    identifier: IdentifierAst | TokenAst
    assignment_token: TokenAst
    value: ExpressionAst

    def print(self) -> str:
        return f"{self.identifier.print()}{self.assignment_token.print()}{self.value.print()}"


ObjectInitializerArgumentAst = (
        ObjectInitializerArgumentNormalAst |
        ObjectInitializerArgumentNamedAst)


@dataclass
class ObjectInitializerArgumentGroupAst(Ast):
    brace_l_token: TokenAst
    arguments: List[ObjectInitializerArgumentAst]
    brace_r_token: TokenAst

    def print(self) -> str:
        return f"{self.brace_l_token.print()}\n{Seq(self.arguments).print("\n")}\n{self.brace_r_token.print()}"


@dataclass
class ObjectInitializerAst(Ast):
    class_type: TypeAst
    arguments: ObjectInitializerArgumentGroupAst

    def print(self) -> str:
        return f"{self.class_type.print()}{self.arguments.print()}"


@dataclass
class ParenthesizedExpressionAst(Ast):
    paren_l_token: TokenAst
    expression: ExpressionAst
    paren_r_token: TokenAst

    def print(self) -> str:
        return f"{self.paren_l_token.print()}{self.expression.print()}{self.paren_r_token.print()}"


@dataclass
class PatternVariantTupleAst(Ast):
    variable: LocalVariableTupleAst

    def print(self) -> str:
        return f"{self.variable.print()}"


@dataclass
class PatternVariantDestructureAst(Ast):
    variable: LocalVariableDestructureAst

    def print(self) -> str:
        return f"{self.variable.print()}"


@dataclass
class PatternVariantVariableAst(Ast):
    variable: LocalVariableSingleAst

    def print(self) -> str:
        return f"{self.variable.print()}"


@dataclass
class PatternVariantLiteralAst(Ast):
    literal: LiteralAst

    def print(self) -> str:
        return f"{self.literal.print()}"


@dataclass
class PatternVariantBoolMemberAst(Ast):
    expression: PostfixExpressionAst

    def print(self) -> str:
        return f"{self.expression.print()}"


@dataclass
class PatternVariantElseAst(Ast):
    else_token: TokenAst

    def print(self) -> str:
        return f"{self.else_token.print()}"


PatternVariantAst = (
        PatternVariantTupleAst |
        PatternVariantDestructureAst |
        PatternVariantVariableAst |
        PatternVariantLiteralAst |
        PatternVariantBoolMemberAst |
        PatternVariantElseAst)


@dataclass
class PatternBlockAst(Ast):
    comp_operator: Optional[TokenAst]
    patterns: List[PatternVariantAst]
    guard: Optional[PatternGuardAst]
    body: InnerScopeAst[StatementAst]

    def print(self) -> str:
        s = ""
        s += f"{self.comp_operator.print()}" if self.comp_operator else ""
        s += f"{Seq(self.patterns).print(", ")}"
        s += f"{self.guard.print()}" if self.guard else ""
        s += f"{self.body.print()}"
        return s


@dataclass
class PatternGuardAst(Ast, Default):
    guard_token: TokenAst
    expression: ExpressionAst

    @staticmethod
    def default() -> Self:
        return PatternGuardAst(-1, TokenAst.no_tok(), TokenAst.no_tok())

    def print(self) -> str:
        return f"{self.guard_token.print()}{self.expression.print()}"


@dataclass
class PostfixExpressionAst(Ast):
    lhs: ExpressionAst
    op: PostfixExpressionOperatorAst

    def print(self) -> str:
        return f"{self.lhs.print()}{self.op.print()}"


@dataclass
class PostfixExpressionOperatorFunctionCallAst(Ast):
    generic_arguments: Optional[GenericArgumentGroupAst]
    arguments: FunctionArgumentGroupAst
    fold_token: Optional[TokenAst]

    def print(self) -> str:
        s = ""
        s += f"{self.generic_arguments.print()}" if self.generic_arguments else ""
        s += f"{self.arguments.print()}"
        s += f"{self.fold_token.print()}" if self.fold_token else ""
        return s


@dataclass
class PostfixExpressionOperatorMemberAccessAst(Ast):
    dot_token: TokenAst
    identifier: PostfixMemberPartAst

    def print(self) -> str:
        return f"{self.dot_token.print()}{self.identifier.print()}"


@dataclass
class PostfixExpressionOperatorEarlyReturnAst(Ast):
    return_token: TokenAst

    def print(self) -> str:
        return f"{self.return_token.print()}"


PostfixExpressionOperatorAst = (
        PostfixExpressionOperatorFunctionCallAst |
        PostfixExpressionOperatorMemberAccessAst |
        PostfixExpressionOperatorEarlyReturnAst)

PostfixMemberPartAst = (
        IdentifierAst |
        LiteralNumberBase10Ast)


@dataclass
class ProgramAst(Ast, PreProcessor):
    module: ModulePrototypeAst
    eof_token: TokenAst

    def print(self) -> str:
        return f"{self.module.print()}{self.eof_token.print()}"

    def pre_process(self, context: ModulePrototypeAst) -> None:
        # ...
        Seq(self.module.body).for_each(lambda m: m.pre_process(context))


@dataclass
class ResidualInnerScopeAst(Ast, Default):
    else_keyword: TokenAst
    body: InnerScopeAst[StatementAst]

    @staticmethod
    def default() -> Self:
        return ResidualInnerScopeAst(-1, TokenAst.no_tok(), InnerScopeAst.default())

    def print(self) -> str:
        return f"{self.else_keyword.print()}{self.body.print()}"


@dataclass
class ReturnStatementAst(Ast):
    return_keyword: TokenAst
    expression: Optional[ExpressionAst]

    def print(self) -> str:
        s = ""
        s += f"{self.return_keyword.print()}"
        s += f"{self.expression.print()}" if self.expression else ""
        return s


@dataclass
class SupMethodPrototypeAst(FunctionPrototypeAst):
    def pre_process(self, context: SupPrototypeAst) -> None:
        ...


@dataclass
class SupPrototypeNormalAst(Ast, PreProcessor):
    sup_keyword: TokenAst
    generic_parameters: GenericParameterGroupAst
    identifier: IdentifierAst
    where_block: WhereBlockAst
    body: InnerScopeAst[SupMemberAst]

    def print(self) -> str:
        return f"{self.sup_keyword.print()}{self.generic_parameters.print()}{self.identifier.print()} {self.where_block.print()} {self.body.print()}"

    def pre_process(self, context: ModulePrototypeAst) -> None:
        ...


@dataclass
class SupPrototypeInheritanceAst(Ast):
    sup_keyword: TokenAst
    generic_parameters: GenericParameterGroupAst
    super_class: TypeAst
    on_keyword: TokenAst
    identifier: TypeAst
    where_block: WhereBlockAst
    body: InnerScopeAst[SupMemberAst]

    def print(self) -> str:
        return f"{self.sup_keyword.print()}{self.generic_parameters.print()}{self.super_class.print()} {self.on_keyword.print()}{self.identifier.print()} {self.where_block.print()} {self.body.print()}"

    def pre_process(self, context: ModulePrototypeAst) -> None:
        ...


SupPrototypeAst = (
        SupPrototypeNormalAst |
        SupPrototypeInheritanceAst)


@dataclass
class TypedefStatementAst(Ast, PreProcessor):
    use_keyword: TokenAst
    generic_parameters: GenericParameterGroupAst
    old_type_namespace: Optional[TypeNamespaceAst]
    items: TypedefStatementItemAst

    def print(self) -> str:
        s = ""
        s += f"{self.use_keyword.print()}{self.generic_parameters.print()}"
        s += f"{self.old_type_namespace.print()}" if self.old_type_namespace else ""
        s += f"{self.items.print()}"
        return s

    def pre_process(self, context: ModulePrototypeAst) -> None:
        ...


@dataclass
class TypedefStatementSpecificItemAst(Ast):
    old_type: TypeAst
    alias: Optional[TypedefStatementSpecificItemAliasAst]

    def print(self) -> str:
        s = ""
        s += f"{self.old_type.print()}"
        s += f"{self.alias.print()}" if self.alias else ""
        return s


@dataclass
class TypedefStatementSpecificItemsAst(Ast):
    brace_l_token: TokenAst
    aliases: List[TypedefStatementSpecificItemAst]
    brace_r_token: TokenAst

    def print(self) -> str:
        return f"{self.brace_l_token.print()}{Seq(self.aliases).print(", ")}{self.brace_r_token.print()}"


@dataclass
class TypedefStatementAllItemsAst(Ast):
    all_token: TokenAst

    def print(self) -> str:
        return f"{self.all_token.print()}"


TypedefStatementItemAst = (
        TypedefStatementSpecificItemAst |
        TypedefStatementSpecificItemsAst |
        TypedefStatementAllItemsAst)


@dataclass
class TypedefStatementSpecificItemAliasAst(Ast):
    as_keyword: TokenAst
    new_type: TypeAst

    def print(self) -> str:
        return f"{self.as_keyword.print()}{self.new_type.print()}"


@dataclass
class SupTypedefAst(TypedefStatementAst):
    annotations: List[AnnotationAst]

    def print(self) -> str:
        return f"{Seq(self.annotations).print("\n")}\n{super().print()}"

    def pre_process(self, context: SupPrototypeAst) -> None:
        ...


@dataclass
class TokenAst(Ast):
    token: Token

    @staticmethod
    def no_tok() -> Self:
        return TokenAst(-1, Token("", TokenType.NO_TOK))

    def print(self) -> str:
        return self.token.token_metadata + (" " if self.token.token_type.name.startswith("Kw") else "")


@dataclass
class TypeSingleAst(Ast):
    parts: List[TypePartAst]

    def print(self) -> str:
        return f"{Seq(self.parts).print(".")}"

    # def substitute_generics(self, from_ty: TypeAst, to_ty: TypeAst) -> None:
    #     type_parts = [(i, p) for i, p in enumerate(self.parts) if isinstance(p, GenericIdentifierAst)]
    #     i, p = type_parts[0]
    #     if p.identifier == from_ty:
    #         self.parts[i] = to_ty
    #
    #     for i, part in type_parts:
    #         for g in part.generic_arguments.arguments if part.generic_arguments else []:
    #             g.type.substitute_generics(from_ty, to_ty)


@dataclass
class TypeTupleAst(Ast):
    paren_l_token: TokenAst
    items: List[TypeAst]
    paren_r_token: TokenAst

    def print(self) -> str:
        return f"{self.paren_l_token.print()}{Seq(self.items).print(", ")}{self.paren_r_token.print()}"

    # def substitute_generics(self, from_ty: TypeAst, to_ty: TypeAst) -> None:
    #     Seq(self.items).for_each(lambda i: i.substitute_generics(from_ty, to_ty))


@dataclass
class TypeUnionAst(Ast):
    items: List[TypeAst]

    def print(self) -> str:
        return f"{Seq(self.items).print(" | ")}"

    # def substitute_generics(self, from_ty: TypeAst, to_ty: TypeAst) -> None:
    #     Seq(self.items).for_each(lambda i: i.substitute_generics(from_ty, to_ty))


TypeAst = (
        TypeSingleAst |
        TypeTupleAst |
        TypeUnionAst)

TypePartAst = (
        IdentifierAst |
        GenericIdentifierAst |
        LiteralNumberBase10Ast)


@dataclass
class TypeNamespaceAst(Ast, Default):
    items: List[TypePartAst]

    @staticmethod
    def default() -> Self:
        return TypeNamespaceAst(-1, [])

    def print(self) -> str:
        return f"{Seq(self.items).print(".")}"


@dataclass
class UnaryExpressionAst(Ast):
    op: UnaryOperatorAst
    rhs: ExpressionAst

    def print(self) -> str:
        return f"{self.op.print()}{self.rhs.print()}"


UnaryOperatorAst = TokenAst


@dataclass
class WhereBlockAst(Ast, Default):
    where_keyword: TokenAst
    constraint_group: WhereConstraintsGroupAst

    @staticmethod
    def default() -> Self:
        return WhereBlockAst(-1, TokenAst.no_tok(), WhereConstraintsAst.default)

    def print(self) -> str:
        return f"{self.where_keyword.print()}{self.constraint_group.print()}"


@dataclass
class WhereConstraintsGroupAst(Ast, Default):
    paren_l_token: TokenAst
    constraints: List[WhereConstraintsAst]
    paren_r_token: TokenAst

    @staticmethod
    def default() -> Self:
        return WhereConstraintsGroupAst(-1, TokenAst.no_tok(), [], TokenAst.no_tok())

    def print(self) -> str:
        return f"{self.paren_l_token.print()}{Seq(self.constraints).print(", ")}{self.paren_r_token.print()}"


@dataclass
class WhereConstraintsAst(Ast, Default):
    types_to_constrain: List[TypeAst]
    colon_token: TokenAst
    constraints: List[TypeAst]

    @staticmethod
    def default() -> Self:
        return WhereConstraintsAst(-1, [], TokenAst.no_tok(), [])

    def print(self) -> str:
        return f"{Seq(self.types_to_constrain).print(", ")}{self.colon_token.print()} {Seq(self.constraints).print(", ")}"


@dataclass
class WhileExpressionAst(Ast):
    while_keyword: TokenAst
    condition: ExpressionAst
    body: InnerScopeAst[StatementAst]
    else_block: Optional[ResidualInnerScopeAst]

    def print(self) -> str:
        s = ""
        s += f"{self.while_keyword.print()}{self.condition.print()} {self.body.print()}"
        s += f"{self.else_block.print()}" if self.else_block else ""
        return s


@dataclass
class WithExpressionAliasAst(Ast, Default):
    variable: LocalVariableAst
    assign_token: TokenAst

    @staticmethod
    def default() -> Self:
        return WithExpressionAliasAst(-1, None, TokenAst.no_tok())

    def print(self) -> str:
        return f"{self.variable.print()}{self.assign_token.print()}"


@dataclass
class WithExpressionAst(Ast):
    with_keyword: TokenAst
    alias: Optional[WithExpressionAliasAst]
    expression: ExpressionAst
    body: InnerScopeAst[StatementAst]

    def print(self) -> str:
        s = ""
        s += f"{self.with_keyword.print()}"
        s += f"{self.alias.print()}" if self.alias else ""
        s += f"{self.expression.print()} {self.body.print()}"
        return s


@dataclass
class YieldExpressionAst(Ast):
    yield_keyword: TokenAst
    with_keyword: Optional[TokenAst]
    convention: ConventionAst
    expression: ExpressionAst

    def print(self) -> str:
        s = ""
        s += f"{self.yield_keyword.print()}"
        s += f"{self.with_keyword.print()}" if self.with_keyword else ""
        s += f"{self.convention.print()}{self.expression.print()}"
        return s


PrimaryExpressionAst = (
        LiteralAst |
        IdentifierAst |
        LambdaPrototypeAst |
        ParenthesizedExpressionAst |
        ObjectInitializerAst |
        TypeAst |
        IfExpressionAst |
        WhileExpressionAst |
        YieldExpressionAst |
        WithExpressionAst |
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

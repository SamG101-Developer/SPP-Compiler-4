from __future__ import annotations

import copy
import dataclasses
import hashlib
import json_fix
from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import List, Optional, Self

from SemanticAnalysis.Symbols.Scopes import ScopeHandler
from SemanticAnalysis.Symbols.Symbols import TypeSymbol, VariableSymbol
from src.SemanticAnalysis.ASTs.AstPrinter import AstPrinter, ast_printer_method, ast_printer_method_indent
from src.SemanticAnalysis.PreProcessor import PreProcessor
from src.SemanticAnalysis.CommonTypes import CommonTypes
from src.SemanticAnalysis.Symbols.SymbolGeneration import SymbolGenerator

from src.LexicalAnalysis.Tokens import Token, TokenType
from src.Utils.Sequence import Seq


@dataclass
class Ast(ABC):
    pos: int
    
    @ast_printer_method
    @abstractmethod
    def print(self, printer: AstPrinter) -> str:
        ...
    
    def __eq__(self, other):
        return isinstance(other, Ast)


@dataclass
class AnnotationAst(Ast):
    at_token: TokenAst
    identifier: ModuleIdentifierAst
    generic_arguments: GenericArgumentGroupAst
    arguments: FunctionArgumentGroupAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.at_token.print(printer)}{self.identifier.print(printer)}{self.generic_arguments.print(printer)}{self.arguments.print(printer)}"


@dataclass
class AssignmentStatementAst(Ast):
    lhs: List[ExpressionAst]
    op: TokenAst
    rhs: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.lhs).print(printer, ", ")} {self.op.print(printer)} {self.rhs.print(printer)}"


@dataclass
class BinaryExpressionAst(Ast):
    lhs: ExpressionAst
    op: TokenAst
    rhs: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.lhs.print(printer)} {self.op.print(printer)} {self.rhs.print(printer)}"


@dataclass
class ClassAttributeAst(Ast):
    annotations: List[AnnotationAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    type_declaration: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.annotations).print(printer, "\n")}{self.identifier.print(printer)}{self.colon_token.print(printer)} {self.type_declaration.print(printer)}"


@dataclass
class ClassPrototypeAst(Ast, PreProcessor, SymbolGenerator):
    annotations: List[AnnotationAst]
    class_token: TokenAst
    identifier: TypeAst
    generic_parameters: Optional[GenericParameterGroupAst]
    where_block: Optional[WhereBlockAst]
    body: InnerScopeAst[ClassAttributeAst]
    _mod: ModuleIdentifierAst = dataclasses.field(default=None)

    def __post_init__(self):
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))
        self.where_block = self.where_block or WhereBlockAst(-1, TokenAst.dummy(TokenType.KwWhere), WhereConstraintsGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR)))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}{self.class_token.print(printer)}{self.identifier.print(printer)}"
        s += f"{self.generic_parameters.print(printer)}" if self.generic_parameters else ""
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    def pre_process(self, context: ModulePrototypeAst) -> None:
        Seq(self.body.members).for_each(lambda m: m.type_declaration.substitute_generics(CommonTypes.self(), self.identifier))
        Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), self.identifier))
        self._mod = context.identifier

    def generate(self, s: ScopeHandler) -> None:
        s.next_scope(self.identifier)
        Seq(self.body.members).for_each(lambda m: s.current_scope.add_symbol(VariableSymbol(m.identifier, m.type_declaration)))
        Seq(self.generic_parameters.parameters).for_each(lambda p: s.current_scope.add_symbol(TypeSymbol(p.identifier, None)))
        s.current_scope.add_symbol(TypeSymbol(IdentifierAst(-1, "Self"), copy.deepcopy(self.identifier)))
        s.prev_scope()


@dataclass
class ConventionMovAst(Ast):
    def print(self, printer: AstPrinter) -> str:
        return f""


@dataclass
class ConventionRefAst(Ast):
    ampersand_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.ampersand_token.print(printer)}"


@dataclass
class ConventionMutAst(Ast):
    ampersand_token: TokenAst
    mut_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.ampersand_token.print(printer)}{self.mut_token.print(printer)}"


ConventionAst = (
        ConventionMovAst |
        ConventionRefAst |
        ConventionMutAst)


@dataclass
class FunctionArgumentNormalAst(Ast):
    convention: ConventionAst
    unpack_token: Optional[TokenAst]
    value: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.convention.print(printer)}"
        s += f"{self.unpack_token.print(printer)}" if self.unpack_token else ""
        s += f"{self.value.print(printer)}"
        return s


@dataclass
class FunctionArgumentNamedAst(Ast):
    identifier: IdentifierAst
    assignment_token: TokenAst
    convention: ConventionAst
    value: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}{self.assignment_token.print(printer)}{self.convention.print(printer)}{self.value.print(printer)}"


FunctionArgumentAst = (
        FunctionArgumentNormalAst |
        FunctionArgumentNamedAst)


@dataclass
class FunctionArgumentGroupAst(Ast):
    paren_l_token: TokenAst
    arguments: List[FunctionArgumentAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.arguments).print(printer, ", ")}{self.paren_r_token.print(printer)}"


@dataclass
class FunctionParameterSelfAst(Ast):
    is_mutable: Optional[TokenAst]
    convention: ConventionAst
    identifier: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.convention.print(printer)} {self.identifier.print(printer)}"
        return s


@dataclass
class FunctionParameterRequiredAst(Ast):
    is_mutable: Optional[TokenAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: ConventionAst
    type_declaration: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.identifier.print(printer)}{self.colon_token.print(printer)} {self.convention.print(printer)}{self.type_declaration.print(printer)}"
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

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.identifier.print(printer)}{self.colon_token.print(printer)} {self.convention.print(printer)}{self.type_declaration.print(printer)} {self.assignment_token.print(printer)} {self.default_value.print(printer)}"
        return s


@dataclass
class FunctionParameterVariadicAst(Ast):
    is_mutable: Optional[TokenAst]
    variadic_token: TokenAst
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: ConventionAst
    type_declaration: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.variadic_token.print(printer)}{self.identifier.print(printer)}{self.colon_token.print(printer)} {self.convention.print(printer)}{self.type_declaration.print(printer)}"
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

    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.parameters).print(printer, ", ")}{self.paren_r_token.print(printer)}"


@dataclass
class FunctionPrototypeAst(Ast, PreProcessor, SymbolGenerator):
    annotations: List[AnnotationAst]
    fun_token: TokenAst
    identifier: IdentifierAst
    generic_parameters: Optional[GenericParameterGroupAst]
    parameters: FunctionParameterGroupAst
    arrow_token: TokenAst
    return_type: TypeAst
    where_block: Optional[WhereBlockAst]
    body: InnerScopeAst[StatementAst]
    _fn_type: TypeAst = dataclasses.field(default=None)

    def __post_init__(self):
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))
        self.where_block = self.where_block or WhereBlockAst(-1, TokenAst.dummy(TokenType.KwWhere), WhereConstraintsGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR)))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}{self.fun_token.print(printer)}{self.identifier.print(printer)}"
        s += f"{self.generic_parameters.print(printer)}" if self.generic_parameters else ""
        s += f"{self.parameters.print(printer)} {self.arrow_token.print(printer)} {self.return_type.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    def pre_process(self, context: ModulePrototypeAst | SupPrototypeAst) -> None:
        if not isinstance(context, ModulePrototypeAst):
            Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), context.identifier))
            Seq(self.parameters.parameters).for_each(lambda p: p.type_declaration.substitute_generics(CommonTypes.self(), context.identifier))
            self.return_type.substitute_generics(CommonTypes.self(), context.identifier)

        mock_class_name = IdentifierAst(-1, f"__MOCK_{self.identifier.value}")
        mock_class_name = TypeSingleAst(-1, [GenericIdentifierAst(-1, mock_class_name.value, None)])

        function_class_type = self._deduce_function_class_type(context)
        function_call_name  = self._deduce_function_call_name(function_class_type)

        if Seq(context.body.members).filter(lambda m: isinstance(m, ClassPrototypeAst) and m.identifier == mock_class_name).empty():
            mock_class_ast = ClassPrototypeAst(
                pos=-1,
                annotations=[],
                class_token=TokenAst.dummy(TokenType.KwCls),
                identifier=mock_class_name,
                generic_parameters=None,
                where_block=None,
                body=InnerScopeAst(
                    pos=-1,
                    brace_l_token=TokenAst.dummy(TokenType.TkBraceL),
                    members=[],
                    brace_r_token=TokenAst.dummy(TokenType.TkBraceR)))

            mock_let_statement = LetStatementInitializedAst(
                pos=-1,
                let_keyword=TokenAst.dummy(TokenType.KwLet),
                assign_to=LocalVariableSingleAst(
                    pos=-1,
                    is_mutable=None,
                    unpack_token=None,
                    identifier=copy.deepcopy(self.identifier)),
                assign_token=TokenAst.dummy(TokenType.TkAssign),
                value=ObjectInitializerAst(
                    pos=-1,
                    class_type=copy.deepcopy(mock_class_name),
                    arguments=ObjectInitializerArgumentGroupAst(
                        pos=-1,
                        brace_l_token=TokenAst.dummy(TokenType.TkBraceL),
                        arguments=[],
                        brace_r_token=TokenAst.dummy(TokenType.TkBraceR))),
                residual=None,
                _sup_let_type=function_class_type)

            context.body.members.insert(0, mock_let_statement)
            context.body.members.insert(0, mock_class_ast)

        call_method_ast = FunctionPrototypeAst(
            pos=-1,
            annotations=[],
            fun_token=TokenAst.dummy(TokenType.KwFun),
            identifier=function_call_name,
            generic_parameters=None,
            parameters=copy.deepcopy(self.parameters),
            arrow_token=TokenAst.dummy(TokenType.TkArrowR),
            return_type=copy.deepcopy(self.return_type),
            where_block=None,
            body=copy.deepcopy(self.body))

        sup_block_ast = SupPrototypeInheritanceAst(
            pos=-1,
            sup_keyword=TokenAst.dummy(TokenType.KwSup),
            generic_parameters=None,
            super_class=copy.deepcopy(function_class_type),
            on_keyword=TokenAst.dummy(TokenType.KwOn),
            identifier=mock_class_name,
            where_block=copy.deepcopy(self.where_block),
            body=InnerScopeAst(
                pos=-1,
                brace_l_token=TokenAst.dummy(TokenType.TkBraceL),
                members=[call_method_ast],
                brace_r_token=TokenAst.dummy(TokenType.TkBraceR)))

        context.body.members.insert(0, sup_block_ast)
        context.body.members.remove(self)
        self._fn_type = function_class_type

    def _deduce_function_class_type(self, context: ModulePrototypeAst | SupPrototypeAst) -> TypeAst:
        is_method = not isinstance(context, ModulePrototypeAst)
        has_self_parameter = isinstance(self.parameters.parameters[0], FunctionParameterSelfAst)
        parameter_types = Seq(self.parameters.parameters).map(lambda p: p.type_declaration).value
        return_type = self.return_type

        if is_method and has_self_parameter:
            convention = self.parameters.parameters[0].convention
            match convention:
                case ConventionRefAst(): return CommonTypes.fun_ref(return_type, parameter_types)
                case ConventionMutAst(): return CommonTypes.fun_mut(return_type, parameter_types)
                case ConventionMovAst(): return CommonTypes.fun_one(return_type, parameter_types)
                case _: raise SystemExit(f"Unknown convention '{convention}' being deduced. Report as bug.")
        else:
            return CommonTypes.fun_ref(return_type, parameter_types)

    def _deduce_function_call_name(self, function_class_type: TypeAst) -> IdentifierAst:
        match function_class_type.parts[-1].value:
            case "FunRef": return IdentifierAst(-1, "call_ref")
            case "FunMut": return IdentifierAst(-1, "call_mut")
            case "FunOne": return IdentifierAst(-1, "call_one")
            case _: raise SystemExit(f"Unknown function class type '{function_class_type}' being deduced. Report as bug.")

    def generate(self, s: ScopeHandler) -> None:
        s.next_scope(self.identifier)
        Seq(self.generic_parameters.parameters).for_each(lambda p: s.current_scope.add_symbol(TypeSymbol(p.identifier, None)))
        s.current_scope.add_symbol(VariableSymbol(copy.deepcopy(self.identifier), self._fn_type))
        s.prev_scope()


@dataclass
class GenericArgumentNormalAst(Ast):
    type: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.type.print(printer)}"

    def __eq__(self, other):
        return isinstance(other, GenericArgumentNormalAst) and self.type == other.type


@dataclass
class GenericArgumentNamedAst(GenericArgumentNormalAst):
    identifier: IdentifierAst
    assignment_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}{self.assignment_token.print(printer)}{self.type.print(printer)}"

    def __eq__(self, other):
        return isinstance(other, GenericArgumentNamedAst) and self.identifier == other.identifier and self.type == other.type


GenericArgumentAst = (
        GenericArgumentNormalAst |
        GenericArgumentNamedAst)


@dataclass
class GenericArgumentGroupAst(Ast):
    bracket_l_token: TokenAst
    arguments: List[GenericArgumentAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.bracket_l_token.print(printer)}{Seq(self.arguments).print(printer, ", ")}{self.bracket_r_token.print(printer)}"

    def __eq__(self, other):
        return isinstance(other, GenericArgumentGroupAst) and self.arguments == other.arguments


@dataclass
class GenericIdentifierAst(Ast):
    value: str
    generic_arguments: Optional[GenericArgumentGroupAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.value}"
        s += f"{self.generic_arguments.print(printer)}" if self.generic_arguments else ""
        return s

    def __eq__(self, other):
        return isinstance(other, GenericIdentifierAst) and self.value == other.value and self.generic_arguments == other.generic_arguments


@dataclass
class GenericParameterRequiredAst(Ast):
    identifier: TypeAst
    inline_constraints: Optional[GenericParameterInlineConstraintAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.inline_constraints.print(printer)}" if self.inline_constraints else ""
        return s


@dataclass
class GenericParameterOptionalAst(GenericParameterRequiredAst):
    assignment_token: TokenAst
    default_value: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{super().print(printer)} {self.assignment_token.print(printer)} {self.default_value.print(printer)}"


@dataclass
class GenericParameterVariadicAst(GenericParameterRequiredAst):
    variadic_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variadic_token.print(printer)}{super().print(printer)}"


GenericParameterAst = (
        GenericParameterRequiredAst |
        GenericParameterOptionalAst |
        GenericParameterVariadicAst)


@dataclass
class GenericParameterGroupAst(Ast):
    bracket_l_token: TokenAst
    parameters: List[GenericParameterAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.bracket_l_token.print(printer)}{Seq(self.parameters).print(printer, ", ")}{self.bracket_r_token.print(printer)}"

    def get_req(self) -> List[GenericParameterRequiredAst]:
        return [p for p in self.parameters if isinstance(p, GenericParameterRequiredAst)]

    def get_opt(self) -> List[GenericParameterOptionalAst]:
        return [p for p in self.parameters if isinstance(p, GenericParameterOptionalAst)]

    def get_var(self) -> List[GenericParameterVariadicAst]:
        return [p for p in self.parameters if isinstance(p, GenericParameterVariadicAst)]


@dataclass
class GenericParameterInlineConstraintAst(Ast):
    colon_token: TokenAst
    constraints: List[TypeAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.colon_token.print(printer)} {Seq(self.constraints).print(printer, ", ")}"


@dataclass
class IdentifierAst(Ast):
    value: str

    def print(self, printer: AstPrinter) -> str:
        return f"{self.value}"

    def __eq__(self, other):
        return isinstance(other, IdentifierAst) and self.value == other.value

    def __hash__(self):
        return int.from_bytes(hashlib.md5(self.value.encode()).digest())

    def __json__(self) -> str:
        return self.value


@dataclass
class IfExpressionAst(Ast):
    if_keyword: TokenAst
    condition: ExpressionAst
    comp_operator: TokenAst
    branches: List[PatternBlockAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.if_keyword.print(printer)}{self.condition.print(printer)} {self.comp_operator.print(printer)} {Seq(self.branches).print(printer, "\n")}"


@dataclass
class InnerScopeAst[T](Ast):
    brace_l_token: TokenAst
    members: List[T]
    brace_r_token: TokenAst

    @ast_printer_method_indent
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.brace_l_token.print(printer)}"
        s += f"\n{Seq(self.members).print(printer, "\n")}\n" if self.members else ""
        s += f"{self.brace_r_token.print(printer)}"
        return s


@dataclass
class LambdaCaptureBlockAst(Ast):
    with_keyword: TokenAst
    bracket_l_token: TokenAst
    items: List[LambdaCaptureItemAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.with_keyword.print(printer)}{self.bracket_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.bracket_r_token.print(printer)}"


@dataclass
class LambdaCaptureItemNormalAst(Ast):
    convention: ConventionAst
    value: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.convention.print(printer)}{self.value.print(printer)}"


@dataclass
class LambdaCaptureItemNamedAst(Ast):
    identifier: IdentifierAst
    assignment_token: TokenAst
    convention: ConventionAst
    value: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}{self.assignment_token.print(printer)}{self.convention.print(printer)}{self.value.print(printer)}"


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

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}\n{self.fun_token.print(printer)}{self.generic_parameters.print(printer)}{self.parameters.print(printer)} {self.arrow_token.print(printer)} {self.return_type.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.lambda_capture_block.print(printer)}" if self.lambda_capture_block else ""
        s += f"{self.body.print(printer)}"
        return s


@dataclass
class LetStatementInitializedAst(Ast, PreProcessor, SymbolGenerator):
    let_keyword: TokenAst
    assign_to: LocalVariableAst
    assign_token: TokenAst
    value: ExpressionAst
    residual: Optional[ResidualInnerScopeAst]
    _sup_let_type: TypeAst = dataclasses.field(default=None)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.let_keyword.print(printer)}{self.assign_to.print(printer)} {self.assign_token.print(printer)} {self.value.print(printer)}"
        s += f"{self.residual.print(printer)}" if self.residual else ""
        return s

    def pre_process(self, context) -> None:
        pass

    def generate(self, s: ScopeHandler) -> None:
        s.current_scope.add_symbol(VariableSymbol(self.assign_to.identifier, self._sup_let_type))


@dataclass
class LetStatementUninitializedAst(Ast):
    let_keyword: TokenAst
    assign_to: LocalVariableAst
    colon_token: TokenAst
    type_declaration: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.let_keyword.print(printer)}{self.assign_to.print(printer)}{self.colon_token.print(printer)} {self.type_declaration.print(printer)}"


LetStatementAst = (
        LetStatementInitializedAst |
        LetStatementUninitializedAst)


@dataclass
class LiteralNumberBase10Ast(Ast):
    sign: Optional[TokenAst]
    number: TokenAst
    primitive_type: Optional[IdentifierAst]  # TypeAst?

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.sign.print(printer)}" if self.sign else ""
        s += f"{self.number.print(printer)}"
        s += f"{self.primitive_type.print(printer)}" if self.primitive_type else ""
        return s

    def __eq__(self, other):
        return isinstance(other, LiteralNumberBase10Ast) and self.sign == other.sign and self.number == other.number and self.primitive_type == other.primitive_type


@dataclass
class LiteralNumberBase02Ast(Ast):
    integer: TokenAst
    primitive_type: Optional[IdentifierAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.integer.print(printer)}"
        s += f"{self.primitive_type.print(printer)}" if self.primitive_type else ""
        return s


@dataclass
class LiteralNumberBase16Ast(Ast):
    integer: TokenAst
    primitive_type: Optional[IdentifierAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.integer.print(printer)}"
        s += f"{self.primitive_type.print(printer)}" if self.primitive_type else ""
        return s


LiteralNumberAst = (
        LiteralNumberBase10Ast |
        LiteralNumberBase02Ast |
        LiteralNumberBase16Ast)


@dataclass
class LiteralStringAst(Ast):
    string: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.string.print(printer)}"


@dataclass
class LiteralArrayNonEmptyAst(Ast):
    bracket_l_token: TokenAst
    items: List[ExpressionAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.bracket_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.bracket_r_token.print(printer)}"


@dataclass
class LiteralArrayEmptyAst(Ast):
    bracket_l_token: TokenAst
    element_type: TypeAst
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.bracket_l_token.print(printer)}{self.element_type.print(printer)}{self.bracket_r_token.print(printer)}"


LiteralArrayAst = (
        LiteralArrayNonEmptyAst |
        LiteralArrayEmptyAst)


@dataclass
class LiteralTupleAst(Ast):
    paren_l_token: TokenAst
    items: List[ExpressionAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.paren_r_token.print(printer)}"


@dataclass
class LiteralBooleanAst(Ast):
    boolean: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.boolean.print(printer)}"


@dataclass
class LiteralRegexAst(Ast):
    regex: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.regex.print(printer)}"


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

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.unpack_token.print(printer)}" if self.unpack_token else ""
        s += f"{self.identifier.print(printer)}"
        return s


@dataclass
class LocalVariableTupleAst(Ast):
    paren_l_token: TokenAst
    items: List[LocalVariableSingleAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.paren_r_token.print(printer)}"


@dataclass
class LocalVariableDestructureAst(Ast):
    class_type: TypeAst
    bracket_l_token: TokenAst
    items: List[LocalVariableSingleAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.class_type.print(printer)}{self.bracket_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.bracket_r_token.print(printer)}"


LocalVariableAst = (
        LocalVariableSingleAst |
        LocalVariableTupleAst |
        LocalVariableDestructureAst)


@dataclass
class ModuleIdentifierAst(Ast):
    parts: List[IdentifierAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.parts).print(printer, ".")}"


@dataclass
class ModulePrototypeAst(Ast):
    annotations: List[AnnotationAst]
    module_keyword: TokenAst
    identifier: ModuleIdentifierAst
    body: ModuleImplementationAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.annotations).print(printer, "\n")}\n{self.module_keyword.print(printer)}{self.identifier.print(printer)}\n{self.body.print(printer)}"


@dataclass
class ModuleImplementationAst(Ast):
    members: List[ModuleMemberAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.members).print(printer, "\n\n")}"


@dataclass
class ObjectInitializerArgumentNormalAst(Ast):
    identifier: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}"


@dataclass
class ObjectInitializerArgumentNamedAst(Ast):
    identifier: IdentifierAst | TokenAst
    assignment_token: TokenAst
    value: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}{self.assignment_token.print(printer)}{self.value.print(printer)}"


ObjectInitializerArgumentAst = (
        ObjectInitializerArgumentNormalAst |
        ObjectInitializerArgumentNamedAst)


@dataclass
class ObjectInitializerArgumentGroupAst(Ast):
    brace_l_token: TokenAst
    arguments: List[ObjectInitializerArgumentAst]
    brace_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.brace_l_token.print(printer)}"
        s += f"\n{Seq(self.arguments).print(printer, "\n")}\n" if self.arguments else ""
        s += f"{self.brace_r_token.print(printer)}"
        return s


@dataclass
class ObjectInitializerAst(Ast):
    class_type: TypeAst
    arguments: ObjectInitializerArgumentGroupAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.class_type.print(printer)}{self.arguments.print(printer)}"


@dataclass
class ParenthesizedExpressionAst(Ast):
    paren_l_token: TokenAst
    expression: ExpressionAst
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{self.expression.print(printer)}{self.paren_r_token.print(printer)}"


@dataclass
class PatternVariantTupleAst(Ast):
    variable: LocalVariableTupleAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variable.print(printer)}"


@dataclass
class PatternVariantDestructureAst(Ast):
    variable: LocalVariableDestructureAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variable.print(printer)}"


@dataclass
class PatternVariantVariableAst(Ast):
    variable: LocalVariableSingleAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variable.print(printer)}"


@dataclass
class PatternVariantLiteralAst(Ast):
    literal: LiteralAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.literal.print(printer)}"


@dataclass
class PatternVariantBoolMemberAst(Ast):
    expression: PostfixExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.expression.print(printer)}"


@dataclass
class PatternVariantElseAst(Ast):
    else_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.else_token.print(printer)}"


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

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.comp_operator.print(printer)}" if self.comp_operator else ""
        s += f"{Seq(self.patterns).print(printer, ", ")}"
        s += f"{self.guard.print(printer)}" if self.guard else ""
        s += f"{self.body.print(printer)}"
        return s


@dataclass
class PatternGuardAst(Ast):
    guard_token: TokenAst
    expression: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.guard_token.print(printer)}{self.expression.print(printer)}"


@dataclass
class PostfixExpressionAst(Ast):
    lhs: ExpressionAst
    op: PostfixExpressionOperatorAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.lhs.print(printer)}{self.op.print(printer)}"


@dataclass
class PostfixExpressionOperatorFunctionCallAst(Ast):
    generic_arguments: Optional[GenericArgumentGroupAst]
    arguments: FunctionArgumentGroupAst
    fold_token: Optional[TokenAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.generic_arguments.print(printer)}" if self.generic_arguments else ""
        s += f"{self.arguments.print(printer)}"
        s += f"{self.fold_token.print(printer)}" if self.fold_token else ""
        return s


@dataclass
class PostfixExpressionOperatorMemberAccessAst(Ast):
    dot_token: TokenAst
    identifier: PostfixMemberPartAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.dot_token.print(printer)}{self.identifier.print(printer)}"


@dataclass
class PostfixExpressionOperatorEarlyReturnAst(Ast):
    return_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.return_token.print(printer)}"


PostfixExpressionOperatorAst = (
        PostfixExpressionOperatorFunctionCallAst |
        PostfixExpressionOperatorMemberAccessAst |
        PostfixExpressionOperatorEarlyReturnAst)

PostfixMemberPartAst = (
        IdentifierAst |
        LiteralNumberBase10Ast)


@dataclass
class ProgramAst(Ast, PreProcessor, SymbolGenerator):
    module: ModulePrototypeAst
    eof_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.module.print(printer)}"

    def pre_process(self, context: ModulePrototypeAst) -> None:
        Seq(self.module.body.members).for_each(lambda m: m.pre_process(context))

    def generate(self, s: ScopeHandler) -> None:
        Seq(self.module.body.members).for_each(lambda m: m.generate(s))


@dataclass
class ResidualInnerScopeAst(Ast):
    else_keyword: TokenAst
    body: InnerScopeAst[StatementAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.else_keyword.print(printer)}{self.body.print(printer)}"


@dataclass
class ReturnStatementAst(Ast):
    return_keyword: TokenAst
    expression: Optional[ExpressionAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.return_keyword.print(printer)}"
        s += f"{self.expression.print(printer)}" if self.expression else ""
        return s


@dataclass
class SupMethodPrototypeAst(FunctionPrototypeAst):
    ...


@dataclass
class SupPrototypeNormalAst(Ast, PreProcessor, SymbolGenerator):
    sup_keyword: TokenAst
    generic_parameters: GenericParameterGroupAst
    identifier: TypeAst
    where_block: WhereBlockAst
    body: InnerScopeAst[SupMemberAst]

    def __post_init__(self):
        self.generic_parameters = self.generic_parameters or GenericParameterGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))
        self.where_block = self.where_block or WhereBlockAst(-1, TokenAst.dummy(TokenType.KwWhere), WhereConstraintsGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR)))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.sup_keyword.print(printer)}{self.generic_parameters.print(printer)}{self.identifier.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    def pre_process(self, context: ModulePrototypeAst) -> None:
        Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), context.identifier))
        Seq(self.body.members).for_each(lambda m: m.pre_process(self))

    def generate(self, s: ScopeHandler) -> None:
        s.next_scope(IdentifierAst(-1, self.identifier.parts[-1].value + "#SUP"))
        Seq(self.body.members).for_each(lambda m: m.generate(s))
        Seq(self.generic_parameters.parameters).for_each(lambda p: s.current_scope.add_symbol(TypeSymbol(p.identifier, None)))
        s.prev_scope()


@dataclass
class SupPrototypeInheritanceAst(SupPrototypeNormalAst):
    super_class: TypeAst
    on_keyword: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.sup_keyword.print(printer)}"
        s += f"{self.generic_parameters.print(printer)}" if self.generic_parameters else ""
        s += f"{self.super_class.print(printer)} {self.on_keyword.print(printer)}{self.identifier.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s


SupPrototypeAst = (
        SupPrototypeNormalAst |
        SupPrototypeInheritanceAst)


@dataclass
class TypedefStatementAst(Ast, PreProcessor):
    use_keyword: TokenAst
    generic_parameters: GenericParameterGroupAst
    old_type_namespace: Optional[TypeNamespaceAst]
    items: TypedefStatementItemAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.use_keyword.print(printer)}{self.generic_parameters.print(printer)}"
        s += f"{self.old_type_namespace.print(printer)}" if self.old_type_namespace else ""
        s += f"{self.items.print(printer)}"
        return s

    def pre_process(self, context: ModulePrototypeAst) -> None:
        Seq(self.generic_parameters.get_opt()).for_each(lambda p: p.default_value.substitute_generics(CommonTypes.self(), context.identifier))
        ...


@dataclass
class TypedefStatementSpecificItemAst(Ast):
    old_type: TypeAst
    alias: Optional[TypedefStatementSpecificItemAliasAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.old_type.print(printer)}"
        s += f"{self.alias.print(printer)}" if self.alias else ""
        return s


@dataclass
class TypedefStatementSpecificItemsAst(Ast):
    brace_l_token: TokenAst
    aliases: List[TypedefStatementSpecificItemAst]
    brace_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.brace_l_token.print(printer)}{Seq(self.aliases).print(printer, ", ")}{self.brace_r_token.print(printer)}"


@dataclass
class TypedefStatementAllItemsAst(Ast):
    all_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.all_token.print(printer)}"


TypedefStatementItemAst = (
        TypedefStatementSpecificItemAst |
        TypedefStatementSpecificItemsAst |
        TypedefStatementAllItemsAst)


@dataclass
class TypedefStatementSpecificItemAliasAst(Ast):
    as_keyword: TokenAst
    new_type: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.as_keyword.print(printer)}{self.new_type.print(printer)}"


@dataclass
class SupTypedefAst(TypedefStatementAst):
    annotations: List[AnnotationAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.annotations).print(printer, "\n")}\n{super().print(printer)}"

    def pre_process(self, context: SupPrototypeAst) -> None:
        ...


@dataclass
class TokenAst(Ast):
    token: Token

    @staticmethod
    def dummy(token_type: TokenType) -> Self:
        return TokenAst(-1, Token(token_type.value, token_type))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return self.token.token_metadata + (" " if self.token.token_type.name.startswith("Kw") else "")


@dataclass
class TypeSingleAst(Ast):
    parts: List[TypePartAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.parts).print(printer, ".")}"

    def substitute_generics(self, from_ty: TypeAst, to_ty: TypeAst) -> None:
        printer = AstPrinter()
        type_parts = [(i, p) for i, p in enumerate(self.parts) if isinstance(p, GenericIdentifierAst)]

        i, p = type_parts[0]
        if p == from_ty.parts[0]:
            self.parts[i] = to_ty

        for i, part in type_parts:
            for g in part.generic_arguments.arguments if part.generic_arguments else []:
                g.type.substitute_generics(from_ty, to_ty)

    def __eq__(self, other):
        return isinstance(other, TypeSingleAst) and self.parts == other.parts

    def __json__(self) -> str:
        printer = AstPrinter()
        return self.print(printer)


@dataclass
class TypeTupleAst(Ast):
    paren_l_token: TokenAst
    items: List[TypeAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.paren_r_token.print(printer)}"

    def substitute_generics(self, from_ty: TypeAst, to_ty: TypeAst) -> None:
        Seq(self.items).for_each(lambda i: i.substitute_generics(from_ty, to_ty))

    def __eq__(self, other):
        return isinstance(other, TypeTupleAst) and self.items == other.items


@dataclass
class TypeUnionAst(Ast):
    items: List[TypeAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.items).print(printer, " | ")}"

    def substitute_generics(self, from_ty: TypeAst, to_ty: TypeAst) -> None:
        Seq(self.items).for_each(lambda i: i.substitute_generics(from_ty, to_ty))

    def __eq__(self, other):
        return isinstance(other, TypeUnionAst) and self.items == other.items


TypeAst = (
        TypeSingleAst |
        TypeTupleAst |
        TypeUnionAst)

TypePartAst = (
        IdentifierAst |
        GenericIdentifierAst |
        LiteralNumberBase10Ast)


@dataclass
class TypeNamespaceAst(Ast):
    items: List[TypePartAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.items).print(printer, ".")}"


@dataclass
class UnaryExpressionAst(Ast):
    op: UnaryOperatorAst
    rhs: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.op.print(printer)}{self.rhs.print(printer)}"


UnaryOperatorAst = TokenAst


@dataclass
class WhereBlockAst(Ast):
    where_keyword: TokenAst
    constraint_group: WhereConstraintsGroupAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.where_keyword.print(printer)}{self.constraint_group.print(printer)}"


@dataclass
class WhereConstraintsGroupAst(Ast):
    brack_l_token: TokenAst
    constraints: List[WhereConstraintsAst]
    brack_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.brack_l_token.print(printer)}{Seq(self.constraints).print(printer, ", ")}{self.brack_r_token.print(printer)}"


@dataclass
class WhereConstraintsAst(Ast):
    types_to_constrain: List[TypeAst]
    colon_token: TokenAst
    constraints: List[TypeAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.types_to_constrain).print(printer, ", ")}{self.colon_token.print(printer)} {Seq(self.constraints).print(printer, ", ")}"


@dataclass
class WhileExpressionAst(Ast):
    while_keyword: TokenAst
    condition: ExpressionAst
    body: InnerScopeAst[StatementAst]
    else_block: Optional[ResidualInnerScopeAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.while_keyword.print(printer)}{self.condition.print(printer)} {self.body.print(printer)}"
        s += f"{self.else_block.print(printer)}" if self.else_block else ""
        return s


@dataclass
class WithExpressionAliasAst(Ast):
    variable: LocalVariableAst
    assign_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variable.print(printer)}{self.assign_token.print(printer)}"


@dataclass
class WithExpressionAst(Ast):
    with_keyword: TokenAst
    alias: Optional[WithExpressionAliasAst]
    expression: ExpressionAst
    body: InnerScopeAst[StatementAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.with_keyword.print(printer)}"
        s += f"{self.alias.print(printer)}" if self.alias else ""
        s += f"{self.expression.print(printer)} {self.body.print(printer)}"
        return s


@dataclass
class YieldExpressionAst(Ast):
    yield_keyword: TokenAst
    with_keyword: Optional[TokenAst]
    convention: ConventionAst
    expression: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.yield_keyword.print(printer)}"
        s += f"{self.with_keyword.print(printer)}" if self.with_keyword else ""
        s += f"{self.convention.print(printer)}{self.expression.print(printer)}"
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

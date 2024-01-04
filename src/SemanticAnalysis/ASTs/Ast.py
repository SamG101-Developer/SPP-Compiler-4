from __future__ import annotations

import copy
import dataclasses
import hashlib
import json

import json_fix
from abc import ABC, abstractmethod
from dataclasses import dataclass
from ordered_set import OrderedSet
from typing import List, Optional, Self, Any, Tuple, Type

from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Symbols.Symbols import TypeSymbol, VariableSymbol, MemoryStatus
from src.SemanticAnalysis.Symbols.SymbolGeneration import SymbolGenerator
from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis, BIN_OP_FUNCS
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.ASTs.AstPrinter import AstPrinter, ast_printer_method, ast_printer_method_indent
from src.SemanticAnalysis.TypeInfer import TypeInfer
from src.SemanticAnalysis.PreProcessor import PreProcessor
from src.SemanticAnalysis.CommonTypes import CommonTypes

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

    def __str__(self):
        printer = AstPrinter()
        return self.print(printer)


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
class BinaryExpressionAst(Ast, SemanticAnalysis, TypeInfer):
    lhs: ExpressionAst
    op: TokenAst
    rhs: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.lhs.print(printer)} {self.op.print(printer)} {self.rhs.print(printer)}"

    def _as_function_call(self) -> PostfixExpressionAst:
        function = PostfixExpressionAst(
            pos=self.pos,
            lhs=self.lhs,
            op=PostfixExpressionOperatorMemberAccessAst(
                pos=self.op.pos,
                dot_token=TokenAst.dummy(TokenType.TkDot),
                identifier=IdentifierAst(self.op.pos, IdentifierAst(self.op.pos, BIN_OP_FUNCS[self.op.token.token_type]))))

        function_call = PostfixExpressionAst(
            pos=self.pos,
            lhs=function,
            op=PostfixExpressionOperatorFunctionCallAst(
                pos=self.op.pos,
                arguments=FunctionArgumentGroupAst(
                    pos=self.op.pos,
                    paren_l_token=TokenAst.dummy(TokenType.TkParenL),
                    arguments=[FunctionArgumentNormalAst(
                        pos=self.op.pos,
                        convention=ConventionMovAst(self.rhs.pos),
                        unpack_token=None,
                        value=self.rhs)],
                    paren_r_token=TokenAst.dummy(TokenType.TkParenR)),
                generic_arguments=None,
                fold_token=None))

        return function_call

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Transform the binary expression to a function call. This doesn't have to go in pre-processing, because the
        # transformation is only temporary, and doesn't affect the tree at all. The original binary expression is still
        # used for the rest of the semantic analysis.

        # TODO : special cases for ?? and "is".
        # TODO : special case for ".." as an operand

        function_call = self._as_function_call()
        function_call.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # TODO : special case for ".." as an operand

        function_call = self._as_function_call()
        return function_call.infer_type(scope_handler, **kwargs)


@dataclass
class ClassAttributeAst(Ast, SemanticAnalysis):
    annotations: List[AnnotationAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    type_declaration: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.annotations).print(printer, "\n")}{self.identifier.print(printer)}{self.colon_token.print(printer)} {self.type_declaration.print(printer)}"

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        Seq(self.annotations).for_each(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))
        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)


@dataclass
class ClassPrototypeAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalysis):
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
        sym = TypeSymbol(self.identifier, self)
        s.current_scope.add_symbol(sym)
        s.into_new_scope(self.identifier)
        sym.associated_scope = s.current_scope
        Seq(self.generic_parameters.parameters).for_each(lambda p: s.current_scope.add_symbol(TypeSymbol(p.identifier, None)))
        Seq(self.body.members).for_each(lambda m: s.current_scope.add_symbol(VariableSymbol(m.identifier, m.type_declaration)))
        s.current_scope.add_symbol(TypeSymbol(CommonTypes.self(), self))
        s.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        scope_handler.move_to_next_scope()

        Seq(self.annotations).for_each(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        # self.where_block.do_semantic_analysis(s, scope_handler, **kwargs, **kwargs)
        self.body.do_semantic_analysis(scope_handler, **(kwargs | {"inline-block": True}))

        # Check that no attributes have the same name as each other.
        if Seq(self.body.members).map(lambda m: m.identifier).contains_duplicates():
            duplicate_attributes = Seq(self.body.members).map(lambda m: m.identifier).non_unique_items()[0]
            exception = SemanticError(f"Duplicate attributes '{duplicate_attributes[0]}' found on type '{self.identifier}':")
            exception.add_traceback(duplicate_attributes[0].pos, f"Attribute '{duplicate_attributes[0]}' declared here.")
            exception.add_traceback(duplicate_attributes[1].pos, f"Attribute '{duplicate_attributes[1]}' re-declared here.")
            raise exception

        scope_handler.exit_cur_scope()

    def __json__(self):
        return self.identifier


@dataclass
class ConventionMovAst(Ast):
    def print(self, printer: AstPrinter) -> str:
        return f""

    @staticmethod
    def default() -> str:
        return f""

    def __eq__(self, other):
        return isinstance(other, ConventionMovAst)


@dataclass
class ConventionRefAst(Ast):
    ampersand_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.ampersand_token.print(printer)}"

    @staticmethod
    def default() -> str:
        return f"&"

    def __eq__(self, other):
        return isinstance(other, ConventionRefAst)


@dataclass
class ConventionMutAst(Ast):
    ampersand_token: TokenAst
    mut_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.ampersand_token.print(printer)}{self.mut_token.print(printer)}"

    @staticmethod
    def default() -> str:
        return f"&mut "

    def __eq__(self, other):
        return isinstance(other, ConventionMutAst)


class ConventionNonInitAst:
    @staticmethod
    def default() -> str:
        return f"Uninitialized: "

    def __eq__(self, other):
        return isinstance(other, ConventionNonInitAst)


class ConventionPartInitAst:
    @staticmethod
    def default() -> str:
        return f"Partially-Initialized: "

    def __eq__(self, other):
        return isinstance(other, ConventionPartInitAst)


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
class FunctionArgumentGroupAst(Ast, SemanticAnalysis):
    paren_l_token: TokenAst
    arguments: List[FunctionArgumentAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.arguments).print(printer, ", ")}{self.paren_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check no named arguments have the same name
        named_arguments = Seq(self.arguments).filter(lambda a: isinstance(a, GenericArgumentNamedAst)).map(lambda a: a.identifier)
        if named_arguments.contains_duplicates():
            duplicate_named_arguments = named_arguments.non_unique_items()[0]
            exception = SemanticError(f"Duplicate argument names '{duplicate_named_arguments[0]}' found in function prototype:")
            exception.add_traceback(duplicate_named_arguments[0].pos, f"Parameter '{duplicate_named_arguments[0]}' declared here.")
            exception.add_traceback(duplicate_named_arguments[1].pos, f"Parameter '{duplicate_named_arguments[1]}' re-declared here.")
            raise exception

        # Check argument order is Normal -> Named
        ordering = {FunctionArgumentNormalAst: "Normal", FunctionArgumentNamedAst: "Named"}
        current_classifications = Seq(self.arguments).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError(f"Invalid parameter order in function prototype:")
            exception.add_traceback(difference[-2][1].pos, f"{ordering[difference[-2][0]]} parameter '{difference[-2][1]}' declared here.")
            exception.add_traceback(difference[-1][1].pos, f"{ordering[difference[-1][0]]} parameter '{difference[-1][1]}' declared here.")
            raise exception

        # TODO : Expand tuple into multiple arguments, so that each part is analysed.
        # TODO : Check memory status of symbols too, not just their convention.

        # Begin memory checks here to prevent overlaps of borrows.
        borrows_ref = OrderedSet()
        borrows_mut = OrderedSet()
        # function = kwargs["function-called"]

        for argument in self.arguments:
            argument.value.do_semantic_analysis(scope_handler, **kwargs)

            match argument.value:
                case IdentifierAst(): sym = scope_handler.current_scope.get_symbol(argument.value)
                case PostfixExpressionAst(): sym = scope_handler.current_scope.get_symbol(argument.value.lhs)
                case _: sym = None

            # Check that an argument is initialized before being used: applies to (postfix) identifier only.
            if sym and sym.memory_info.ast_consumed:
                exception = SemanticError(f"Variable '{argument.value}' used before being initialized:")
                exception.add_traceback(sym.memory_info.ast_consumed.pos, f"Variable '{argument.value}' uninitialized/moved here.")
                exception.add_traceback(argument.value.pos, f"Variable '{argument.value}' used here.")
                raise exception

            # Check that an argument is not partially moved before being used: applies to (postfix) identifier only.
            # Non-overlapping partial moves are ok, for example, if "a.b" is moved, "a.c" is fine to use, but "a" isn't.
            if sym and sym.memory_info.ast_partial_moves:
                for existing_partial_move in sym.memory_info.ast_partial_moves:
                    existing_partial_move = str(existing_partial_move)
                    if existing_partial_move.startswith(str(argument.value)):
                        exception = SemanticError(f"Variable '{argument.value}' used after being partially moved:")
                        for partial_move in sym.memory_info.ast_partial_moves:
                            exception.add_traceback(partial_move.pos, f"Variable '{argument.value}' partially moved here.")
                        exception.add_traceback(argument.value.pos, f"Variable '{argument.value}' used here.")
                        raise exception

            # Check conventions of arguments to enforce the law of exclusivity. Note that "&mut a", "&mut a.b" is an
            # overlap, but "&mut a.b", "&mut a.c" is not.
            # TODO: replacing partial moves once fulfilled.
            match argument.convention:
                case ConventionMovAst() if sym:
                    # Mark the symbol as consumed, if the argument is a single identifier.
                    if isinstance(argument.value, IdentifierAst):
                        sym.memory_info.ast_consumed = argument

                    # Cannot move from a borrowed context so enforce this here too.
                    elif sym.memory_info.is_borrow:
                        exception = SemanticError(f"Cannot move from a borrowed context:")
                        exception.add_traceback(sym.memory_info.ast_borrow.pos, f"Variable '{argument.value}' borrowed here.")
                        exception.add_traceback(argument.pos, f"Partial move '{argument}' taken here.")
                        raise exception

                    # Otherwise, mark the left most identifier as partially moved.
                    else:
                        sym.memory_info.ast_partial_moves.append(argument)

                case ConventionMutAst():
                    # Can only take a borrow from a (postfix) identifier.
                    if not sym:
                        exception = SemanticError(f"Cannot take an borrow from a non-identifier:")
                        exception.add_traceback(argument.convention.pos, f"Borrow '{argument.convention}' taken here.")
                        raise exception

                    # Can only take a mutable borrow from a mutable symbol
                    if not sym.is_mutable:
                        exception = SemanticError(f"Cannot take a mutable borrow from an immutable variable:")
                        exception.add_traceback(sym.memory_info.ast_initialized.pos, f"Variable '{argument.value}' declared immutably here.")
                        exception.add_traceback(argument.convention.pos, f"Mutable borrow '{argument.value}' taken here.")
                        raise exception

                    # For a mutable borrow to take place, ensure that no other overlapping part of the variable is
                    # already borrowed immutably or mutably.
                    for i, existing_borrow in borrows_ref | borrows_mut:
                        if existing_borrow.startswith(str(argument.value)):
                            exception = SemanticError(f"Cannot take a mutable borrow to an object '{argument.value}' that's already borrowed:")
                            exception.add_traceback(i, f"Object '{argument.value}' already borrowed here.")
                            exception.add_traceback(argument.convention.pos, f"Object '{argument.value}' attempted to ebe borrowed mutably here.")
                            raise exception

                    borrows_mut.add((argument.value.pos, str(argument.value)))

                case ConventionRefAst():
                    # Can only take a borrow from a (postfix) identifier.
                    if not sym:
                        exception = SemanticError(f"Cannot take a borrow from a non-identifier:")
                        exception.add_traceback(argument.convention.pos, f"Borrow '{argument.convention}' taken here.")
                        raise exception

                    # For an immutable borrow to take place, ensure that no other overlapping part of the variable is
                    # already borrowed mutably.
                    for i, existing_borrow in borrows_mut:
                        existing_borrow_check = str(existing_borrow)
                        if existing_borrow_check.startswith(str(argument.value)):
                            exception = SemanticError(f"Cannot take a immutable borrow to an object '{argument.value}' that's already mutably borrowed:")
                            exception.add_traceback(i, f"Object '{argument.value}' already mutably borrowed here.")
                            exception.add_traceback(argument.convention.pos, f"Object '{argument.value}' attempted to be borrowed immutably here.")
                            raise exception

                    borrows_ref.add((argument.value.pos, str(argument.value)))


@dataclass
class FunctionParameterSelfAst(Ast, SemanticAnalysis):
    is_mutable: Optional[TokenAst]
    convention: ConventionAst
    identifier: IdentifierAst
    type_declaration: TypeAst = dataclasses.field(default=None)

    def __post_init__(self):
        self.type_declaration = CommonTypes.self()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.convention.print(printer)} {self.identifier.print(printer)}"
        return s

    def __eq__(self, other):
        return isinstance(other, FunctionParameterSelfAst)

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        symbol = VariableSymbol(self.identifier, CommonTypes.self(), is_mutable=self.is_mutable is not None, memory_info=MemoryStatus(
            ast_initialized=self,
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self))
        scope_handler.current_scope.add_symbol(symbol)


@dataclass
class FunctionParameterRequiredAst(Ast, SemanticAnalysis):
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

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)
        symbol = VariableSymbol(self.identifier, self.type_declaration, is_mutable=self.is_mutable is not None, memory_info=MemoryStatus(
            ast_initialized=self,
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self))
        scope_handler.current_scope.add_symbol(symbol)

    def __eq__(self, other):
        return isinstance(other, FunctionParameterRequiredAst) and self.identifier == other.identifier


@dataclass
class FunctionParameterOptionalAst(Ast, SemanticAnalysis):
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

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)
        symbol = VariableSymbol(self.identifier, self.type_declaration, is_mutable=self.is_mutable is not None, memory_info=MemoryStatus(
            ast_initialized=self,
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self))
        scope_handler.current_scope.add_symbol(symbol)

        # self.default_value.do_semantic_analysis(s, scope_handler, **kwargs, **kwargs)

        if not isinstance(self.convention, ConventionMovAst):
            exception = SemanticError(f"Optional parameters must use the by-val convention:")
            exception.add_traceback(self.convention.pos, f"Convention '{self.convention}' used here.")
            raise exception

        # if self.type_declaration != self.default_value.infer_type():
        #     exception = SemanticError(f"Optional parameter type does not match default value type:")
        #     exception.add_traceback(self.type_declaration.pos, f"Parameter type '{self.type_declaration}' declared here.")
        #     exception.add_traceback(self.default_value.pos, f"Default value type '{self.default_value.infer_type()}' inferred here.")
        #     raise exception

    def __eq__(self, other):
        return isinstance(other, FunctionParameterOptionalAst) and self.identifier == other.identifier


@dataclass
class FunctionParameterVariadicAst(Ast, SemanticAnalysis):
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

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # TODO : type declaration for variadics will need to be checked later: tuple?
        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)
        symbol = VariableSymbol(self.identifier, self.type_declaration, is_mutable=self.is_mutable is not None, memory_info=MemoryStatus(
            ast_initialized=self,
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self))
        scope_handler.current_scope.add_symbol(symbol)

    def __eq__(self, other):
        return isinstance(other, FunctionParameterVariadicAst) and self.identifier == other.identifier


FunctionParameterAst = (
        FunctionParameterSelfAst |
        FunctionParameterRequiredAst |
        FunctionParameterOptionalAst |
        FunctionParameterVariadicAst)


@dataclass
class FunctionParameterGroupAst(Ast, SemanticAnalysis):
    paren_l_token: TokenAst
    parameters: List[FunctionParameterAst]
    paren_r_token: TokenAst

    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.parameters).print(printer, ", ")}{self.paren_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check no parameters have the same name
        if Seq(self.parameters).map(lambda p: p.identifier).contains_duplicates():
            duplicate_parameters = Seq(self.parameters).map(lambda p: p.identifier).non_unique_items()[0]
            exception = SemanticError(f"Duplicate parameters '{duplicate_parameters[0]}' found in function prototype:")
            exception.add_traceback(duplicate_parameters[0].pos, f"Parameter '{duplicate_parameters[0]}' declared here.")
            exception.add_traceback(duplicate_parameters[1].pos, f"Parameter '{duplicate_parameters[1]}' re-declared here.")
            raise exception

        # Check parameter order is Self -> Required -> Optional -> Variadic
        ordering = {FunctionParameterSelfAst: "Self", FunctionParameterRequiredAst: "Required", FunctionParameterOptionalAst: "Optional", FunctionParameterVariadicAst: "Variadic"}
        current_classifications = Seq(self.parameters).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError(f"Invalid parameter order in function prototype:")
            exception.add_traceback(difference[-2][1].pos, f"{ordering[difference[-2][0]]} parameter '{difference[-2][1]}' declared here.")
            exception.add_traceback(difference[-1][1].pos, f"{ordering[difference[-1][0]]} parameter '{difference[-1][1]}' declared here.")
            raise exception

        # Check that the function is class method, not in the module global space, if there is a "self" parameter
        if self.parameters and isinstance(self.parameters[0], FunctionParameterSelfAst) and scope_handler.at_global_scope(parent_level=2):
            exception = SemanticError(f"Can only use the 'self' parameter within a class:")
            exception.add_traceback(self.parameters[0].pos, f"Parameter '{self.parameters[0]}' declared here.")
            raise exception

        # Check that there is a maximum of 1 variadic parameter
        variadic_parameters = Seq(self.parameters).filter(lambda p: isinstance(p, FunctionParameterVariadicAst))
        if variadic_parameters.length > 1:
            exception = SemanticError(f"Invalid parameter order in function prototype:")
            exception.add_traceback(variadic_parameters[0].pos, f"1st variadic parameter '{variadic_parameters[0]}' declared here.")
            exception.add_traceback(variadic_parameters[1].pos, f"2nd variadic parameter '{variadic_parameters[1]}' declared here.")
            raise exception

        Seq(self.parameters).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))


@dataclass
class FunctionPrototypeAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalysis):
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

            context.body.members.append(mock_class_ast)
            context.body.members.append(mock_let_statement)

        call_method_ast = SupMethodPrototypeAst(
            pos=self.pos,
            annotations=[],
            fun_token=TokenAst.dummy(TokenType.KwFun),
            identifier=function_call_name,
            generic_parameters=None,
            parameters=self.parameters,
            arrow_token=TokenAst.dummy(TokenType.TkArrowR),
            return_type=self.return_type,
            where_block=None,
            body=self.body)

        sup_block_ast = SupPrototypeInheritanceAst(
            pos=-1,
            sup_keyword=TokenAst.dummy(TokenType.KwSup),
            generic_parameters=None,
            super_class=copy.deepcopy(function_class_type),
            on_keyword=TokenAst.dummy(TokenType.KwOn),
            identifier=mock_class_name,
            where_block=self.where_block,
            body=InnerScopeAst(
                pos=-1,
                brace_l_token=TokenAst.dummy(TokenType.TkBraceL),
                members=[call_method_ast],
                brace_r_token=TokenAst.dummy(TokenType.TkBraceR)))

        context.body.members.append(sup_block_ast)
        self._fn_type = function_class_type

    def _deduce_function_class_type(self, context: ModulePrototypeAst | SupPrototypeAst) -> TypeAst:
        is_method = not isinstance(context, ModulePrototypeAst)
        has_self_parameter = self.parameters.parameters and isinstance(self.parameters.parameters[0], FunctionParameterSelfAst)
        parameter_types = Seq(self.parameters.parameters).map(lambda p: p.type_declaration).value
        return_type = self.return_type

        if is_method and has_self_parameter:
            convention = self.parameters.parameters[0].convention
            match convention:
                case ConventionRefAst(): return CommonTypes.fun_ref(return_type, parameter_types, pos=self.pos)
                case ConventionMutAst(): return CommonTypes.fun_mut(return_type, parameter_types, pos=self.pos)
                case ConventionMovAst(): return CommonTypes.fun_one(return_type, parameter_types, pos=self.pos)
                case _: raise SystemExit(f"Unknown convention '{convention}' being deduced. Report as bug.")
        else:
            return CommonTypes.fun_ref(return_type, parameter_types, pos=self.pos)

    def _deduce_function_call_name(self, function_class_type: TypeAst) -> IdentifierAst:
        match function_class_type.parts[-1].value:
            case "FunRef": return IdentifierAst(self.identifier.pos, "call_ref")
            case "FunMut": return IdentifierAst(self.identifier.pos, "call_mut")
            case "FunOne": return IdentifierAst(self.identifier.pos, "call_one")
            case _: raise SystemExit(f"Unknown function class type '{function_class_type}' being deduced. Report as bug.")

    def generate(self, s: ScopeHandler) -> None:
        s.into_new_scope(self.identifier)
        Seq(self.generic_parameters.parameters).for_each(lambda p: s.current_scope.add_symbol(TypeSymbol(p.identifier, None)))
        s.current_scope.add_symbol(VariableSymbol(copy.deepcopy(self.identifier), self._fn_type))
        s.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        scope_handler.move_to_next_scope()

        Seq(self.annotations).for_each(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.return_type.do_semantic_analysis(scope_handler, **kwargs)
        # self.where_block.do_semantic_analysis(s)
        self.body.do_semantic_analysis(scope_handler, **(kwargs | {"target-return-type": self.return_type}))

        # Check that there a return statement at the end fo a non-Void function
        if self.return_type != CommonTypes.void() and self.body.members and not isinstance(self.body.members[-1], ReturnStatementAst):
            exception = SemanticError(f"Missing return statement in non-Void function:")
            exception.add_traceback(self.pos, f"Function '{self.identifier}' declared here.")
            exception.add_traceback(self.body.members[-1].pos, f"Last statement '{self.body.members[-1]}' found here.")
            raise exception

        scope_handler.exit_cur_scope()


@dataclass
class GenericArgumentNormalAst(Ast, SemanticAnalysis):
    type: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.type.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.type.do_semantic_analysis(scope_handler, **kwargs)

    def __eq__(self, other):
        return isinstance(other, GenericArgumentNormalAst) and self.type == other.type


@dataclass
class GenericArgumentNamedAst(GenericArgumentNormalAst, SemanticAnalysis):
    identifier: IdentifierAst
    assignment_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.identifier.print(printer)}{self.assignment_token.print(printer)}{self.type.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        super().do_semantic_analysis(scope_handler, **kwargs)
        self.type.do_semantic_analysis(scope_handler, **kwargs)

    def __eq__(self, other):
        return isinstance(other, GenericArgumentNamedAst) and self.identifier == other.identifier and self.type == other.type


GenericArgumentAst = (
        GenericArgumentNormalAst |
        GenericArgumentNamedAst)


@dataclass
class GenericArgumentGroupAst(Ast, SemanticAnalysis):
    bracket_l_token: TokenAst
    arguments: List[GenericArgumentAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.bracket_l_token.print(printer)}{Seq(self.arguments).print(printer, ", ")}{self.bracket_r_token.print(printer)}" if self.arguments else ""
        return s

    def __eq__(self, other):
        return isinstance(other, GenericArgumentGroupAst) and self.arguments == other.arguments

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check no named arguments have the same name
        named_arguments = Seq(self.arguments).filter(lambda a: isinstance(a, GenericArgumentNamedAst)).map(lambda a: a.identifier)
        if named_arguments.contains_duplicates():
            duplicate_named_arguments = named_arguments.non_unique_items()[0]
            exception = SemanticError(f"Duplicate generic argument names '{duplicate_named_arguments[0]}' found:")
            exception.add_traceback(duplicate_named_arguments[0].pos, f"Argument '{duplicate_named_arguments[0]}' declared here.")
            exception.add_traceback(duplicate_named_arguments[1].pos, f"Argument '{duplicate_named_arguments[1]}' re-declared here.")
            raise exception

        # Check argument order is Normal -> Named
        ordering = {GenericArgumentNormalAst: "Normal", GenericArgumentNamedAst: "Named"}
        current_classifications = Seq(self.arguments).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError(f"Invalid generic argument order:")
            exception.add_traceback(difference[-2][1].pos, f"{ordering[difference[-2][0]]} argument '{difference[-2][1]}' declared here.")
            exception.add_traceback(difference[-1][1].pos, f"{ordering[difference[-1][0]]} argument '{difference[-1][1]}' declared here.")
            raise exception

        Seq(self.arguments).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))


@dataclass
class GenericIdentifierAst(Ast):
    value: str
    generic_arguments: Optional[GenericArgumentGroupAst]

    def __post_init__(self):
        self.generic_arguments = self.generic_arguments or GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.value}"
        s += f"{self.generic_arguments.print(printer)}" if self.generic_arguments else ""
        return s

    def __eq__(self, other):
        return isinstance(other, GenericIdentifierAst) and self.value == other.value and self.generic_arguments == other.generic_arguments


@dataclass
class GenericParameterRequiredAst(Ast, SemanticAnalysis):
    identifier: TypeAst
    inline_constraints: Optional[GenericParameterInlineConstraintAst]

    def __post_init__(self):
        self.identifier = TypeSingleAst(self.identifier.pos, [GenericIdentifierAst(self.identifier.pos, self.identifier.value, None)])

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.inline_constraints.print(printer)}" if self.inline_constraints else ""
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...


@dataclass
class GenericParameterOptionalAst(GenericParameterRequiredAst):
    assignment_token: TokenAst
    default_value: TypeAst

    def __post_init__(self):
        pass

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{super().print(printer)} {self.assignment_token.print(printer)} {self.default_value.print(printer)}"


@dataclass
class GenericParameterVariadicAst(GenericParameterRequiredAst):
    variadic_token: TokenAst

    def __post_init__(self):
        pass

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variadic_token.print(printer)}{super().print(printer)}"


GenericParameterAst = (
        GenericParameterRequiredAst |
        GenericParameterOptionalAst |
        GenericParameterVariadicAst)


@dataclass
class GenericParameterGroupAst(Ast, SemanticAnalysis):
    bracket_l_token: TokenAst
    parameters: List[GenericParameterAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.bracket_l_token.print(printer)}{Seq(self.parameters).print(printer, ", ")}{self.bracket_r_token.print(printer)}" if self.parameters else ""
        return s

    def get_req(self) -> List[GenericParameterRequiredAst]:
        return [p for p in self.parameters if isinstance(p, GenericParameterRequiredAst)]

    def get_opt(self) -> List[GenericParameterOptionalAst]:
        return [p for p in self.parameters if isinstance(p, GenericParameterOptionalAst)]

    def get_var(self) -> List[GenericParameterVariadicAst]:
        return [p for p in self.parameters if isinstance(p, GenericParameterVariadicAst)]

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Check no parameters have the same name
        if Seq(self.parameters).map(lambda p: p.identifier).contains_duplicates():
            duplicate_parameters = Seq(self.parameters).map(lambda p: p.identifier).non_unique_items()[0]
            exception = SemanticError(f"Duplicate parameters '{duplicate_parameters[0]}' found in function prototype:")
            exception.add_traceback(duplicate_parameters[0].pos, f"Parameter '{duplicate_parameters[0]}' declared here.")
            exception.add_traceback(duplicate_parameters[1].pos, f"Parameter '{duplicate_parameters[1]}' re-declared here.")
            raise exception

        # Check parameter order is Self -> Required -> Optional -> Variadic
        ordering = {GenericParameterRequiredAst: "Required", GenericParameterOptionalAst: "Optional", GenericParameterVariadicAst: "Variadic"}
        current_classifications = Seq(self.parameters).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError(f"Invalid generic parameter order:")
            exception.add_traceback(difference[-2][1].pos, f"{ordering[difference[-2][0]]} generic parameter '{difference[-2][1]}' declared here.")
            exception.add_traceback(difference[-1][1].pos, f"{ordering[difference[-1][0]]} generic parameter '{difference[-1][1]}' declared here.")
            raise exception

        Seq(self.parameters).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))


@dataclass
class GenericParameterInlineConstraintAst(Ast):
    colon_token: TokenAst
    constraints: List[TypeAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.colon_token.print(printer)} {Seq(self.constraints).print(printer, ", ")}"


@dataclass
class IdentifierAst(Ast, SemanticAnalysis, TypeInfer):
    value: str

    def print(self, printer: AstPrinter) -> str:
        return f"{self.value}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        if not scope_handler.current_scope.has_symbol(self):
            exception = SemanticError(f"Undefined identifier '{self.value}':")
            exception.add_traceback(self.pos, f"Identifier '{self.value}' used here.")
            raise exception

        scope_handler.current_scope.get_symbol(self).type.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        sym = scope_handler.current_scope.get_symbol(self)
        if sym.memory_info.ast_consumed:
            convention = ConventionNonInitAst
        elif sym.memory_info.ast_partial_moves:
            convention = ConventionPartInitAst
        elif sym.memory_info.is_borrow_mut:
            convention = ConventionMutAst
        elif sym.memory_info.is_borrow_ref:
            convention = ConventionRefAst
        else:
            convention = ConventionMovAst

        return convention, sym.type

    def __eq__(self, other):
        return isinstance(other, IdentifierAst) and self.value == other.value

    def __hash__(self):
        return int.from_bytes(hashlib.md5(self.value.encode()).digest())

    def __json__(self) -> str:
        return self.value


@dataclass
class IfExpressionAst(Ast, SemanticAnalysis, TypeInfer):
    if_keyword: TokenAst
    condition: ExpressionAst
    comp_operator: TokenAst
    branches: List[PatternBlockAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.if_keyword.print(printer)}{self.condition.print(printer)} {self.comp_operator.print(printer)} {Seq(self.branches).print(printer, "\n")}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        scope_handler.into_new_scope("<if-expression>")
        self.condition.do_semantic_analysis(scope_handler, **kwargs)
        Seq(self.branches).for_each(lambda b: b.do_semantic_analysis(scope_handler, **kwargs))

        if kwargs.get("assignment", False):
            if Seq(self.branches).map(lambda b: b.body.members[-1].infer_type(scope_handler)).unique_items().length > 1:
                conflicting_types = Seq(self.branches).map(lambda b: b.body.members[-1].infer_type(
                    scope_handler)).non_unique_items_flat()
                exception = SemanticError(f"Duplicate types '{conflicting_types[0]}' found in assign-if-expression:")
                exception.add_traceback(conflicting_types[0].pos, f"Type '{conflicting_types[0]}' inferred here.")  # TODO : wrong.pos
                exception.add_traceback(conflicting_types[1].pos, f"Type '{conflicting_types[1]}' inferred here.")  # TODO : wrong.pos
                raise exception

            if not self.branches[-1].is_else_branch():
                exception = SemanticError(f"Missing else branch in assign-if-expression:")
                exception.add_traceback(self.pos, f"If-expression declared here.")
                raise exception

        scope_handler.exit_cur_scope()

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        if self.branches and self.branches[0].body.members:
            return self.branches[0].body.members[-1].infer_type(scope_handler)
        return ConventionMovAst, CommonTypes.void()


@dataclass
class InnerScopeAst[T](Ast, SemanticAnalysis):
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

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        if kwargs.get("inline-block", False):
            Seq(self.members).for_each(lambda m: m.do_semantic_analysis(scope_handler, **kwargs))
        else:
            scope_handler.into_new_scope("<inner-scope>")
            Seq(self.members).for_each(lambda m: m.do_semantic_analysis(scope_handler, **kwargs))
            scope_handler.exit_cur_scope()


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
class LetStatementInitializedAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalysis):
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

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        self.value.do_semantic_analysis(scope_handler, **kwargs)
        sym = VariableSymbol(
            name=self.assign_to.identifier,
            type=self.value.infer_type(scope_handler),
            is_mutable=self.assign_to.is_mutable is not None,
            memory_info=MemoryStatus(ast_initialized=self))

        scope_handler.current_scope.add_symbol(sym)


@dataclass
class LetStatementUninitializedAst(Ast, SemanticAnalysis):
    let_keyword: TokenAst
    assign_to: LocalVariableAst
    colon_token: TokenAst
    type_declaration: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.let_keyword.print(printer)}{self.assign_to.print(printer)}{self.colon_token.print(printer)} {self.type_declaration.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        sym = VariableSymbol(
            self.assign_to.identifier,
            self.type_declaration,
            is_mutable=self.assign_to.is_mutable is not None,
            memory_info=MemoryStatus(ast_consumed=self))

        scope_handler.current_scope.add_symbol(sym)


LetStatementAst = (
        LetStatementInitializedAst |
        LetStatementUninitializedAst)


@dataclass
class LiteralNumberBase10Ast(Ast, SemanticAnalysis, TypeInfer):
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

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # TODO: make sure that value < bound of type is its given (don't allow narrowing)
        # TODO: don't allow [float -> int] conversion
        ...

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        if self.primitive_type:
            return ConventionMovAst, TypeSingleAst(pos=self.primitive_type.pos, parts=[GenericIdentifierAst(self.primitive_type.pos, self.primitive_type.value, None)])
        if self.number.token.token_type == TokenType.LxDecFloat:
            return ConventionMovAst, CommonTypes.big_dec(self.pos)
        return ConventionMovAst, CommonTypes.big_num(self.pos)

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
class LiteralStringAst(Ast, SemanticAnalysis, TypeInfer):
    string: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.string.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        pass

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, CommonTypes.str()


@dataclass
class LiteralArrayNonEmptyAst(Ast, SemanticAnalysis):
    bracket_l_token: TokenAst
    items: List[ExpressionAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.bracket_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.bracket_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        Seq(self.items).for_each(lambda i: i.do_semantic_analysis(scope_handler, **kwargs))
        non_matching_types = Seq(self.items).map(lambda i: i.infer_type(scope_handler)).non_unique_items_flat()
        if non_matching_types:
            exception = SemanticError(f"Array items must have the same type:")
            exception.add_traceback(non_matching_types[0].pos, f"Item '{non_matching_types[0]}' found here.")
            exception.add_traceback(non_matching_types[1].pos, f"Item '{non_matching_types[1]}' has a different type.")
            raise exception


@dataclass
class LiteralArrayEmptyAst(Ast, SemanticAnalysis):
    bracket_l_token: TokenAst
    element_type: TypeAst
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.bracket_l_token.print(printer)}{self.element_type.print(printer)}{self.bracket_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.element_type.do_semantic_analysis(scope_handler, **kwargs)


LiteralArrayAst = (
        LiteralArrayNonEmptyAst |
        LiteralArrayEmptyAst)


@dataclass
class LiteralTupleAst(Ast, SemanticAnalysis):
    paren_l_token: TokenAst
    items: List[ExpressionAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{Seq(self.items).print(printer, ", ")}{self.paren_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        Seq(self.items).for_each(lambda i: i.do_semantic_analysis(scope_handler, **kwargs))


@dataclass
class LiteralBooleanAst(Ast, SemanticAnalysis, TypeInfer):
    boolean: bool

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.boolean}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        pass

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, CommonTypes.bool()


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
class ModulePrototypeAst(Ast, SemanticAnalysis):
    annotations: List[AnnotationAst]
    module_keyword: TokenAst
    identifier: ModuleIdentifierAst
    body: ModuleImplementationAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.annotations).print(printer, "\n")}\n{self.module_keyword.print(printer)}{self.identifier.print(printer)}\n{self.body.print(printer)}"

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        Seq(self.annotations).for_each(lambda x: x.do_semantic_analysis(scope_handler, **kwargs))
        # self.identifier.do_semantic_analysis(s)
        self.body.do_semantic_analysis(scope_handler, **kwargs)


@dataclass
class ModuleImplementationAst(Ast, SemanticAnalysis):
    members: List[ModuleMemberAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{Seq(self.members).print(printer, "\n\n")}"

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        Seq(self.members).for_each(lambda x: x.do_semantic_analysis(scope_handler, **kwargs))


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
class ObjectInitializerAst(Ast, SemanticAnalysis, TypeInfer):
    class_type: TypeAst
    arguments: ObjectInitializerArgumentGroupAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.class_type.print(printer)}{self.arguments.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # self.arguments.do_semantic_analysis(scope_handler, **kwargs)

        type_scope = scope_handler.current_scope.get_symbol(self.class_type).associated_scope
        attributes = type_scope.all_symbols(exclusive=True)
        sup_classes = type_scope.exclusive_sup_scopes

        # Check if a default value has been given in "else=":
        default_value_given = (Seq(self.arguments.arguments)
               .filter(lambda a: isinstance(a, ObjectInitializerArgumentNamedAst))
               .filter(lambda a: isinstance(a.identifier, TokenAst) and a.identifier.token.token_type == TokenType.KwElse))

        # Check there is a maximum of 1 default value given:
        if default_value_given.length > 1:
            exception = SemanticError(f"Multiple default values given in object initializer:")
            exception.add_traceback(default_value_given[0].pos, f"1st default value given here.")
            exception.add_traceback(default_value_given[1].pos, f"2nd default value given here.")
            raise exception

        # Check that the default value is of the correct type:
        if default_value_given and self.class_type != default_value_given[0].value.infer_type(scope_handler):
            exception = SemanticError(f"Invalid type '{default_value_given[0].value.infer_type(scope_handler)}' given to object initializer:")
            exception.add_traceback(self.class_type.pos, f"Object initializer declared here with type '{self.class_type}'.")
            exception.add_traceback(default_value_given[0].value.pos, f"Object initializer given value here with type '{default_value_given[0].value.infer_type(scope_handler)}'.")
            raise exception

        # Check that each attribute has been given a value if there is no default value:
        if not default_value_given:
            for attribute in Seq(attributes).filter(lambda s: isinstance(s, VariableSymbol)):
                if not Seq(self.arguments.arguments).map(lambda a: a.identifier).contains(attribute.name):
                    exception = SemanticError(f"Missing attribute '{attribute.name}' in object initializer:")
                    exception.add_traceback(attribute.name.pos, f"Attribute '{attribute.name}' declared here.")
                    exception.add_traceback(self.pos, f"Object initializer declared here.")
                    raise exception

        # Check that each attribute's given value is of the correct type:
        for attribute in Seq(attributes).filter(lambda s: isinstance(s, VariableSymbol)):
            given_argument = Seq(self.arguments.arguments).filter(lambda a: a.identifier == attribute.name)

            # No argument given means that the default value will be used.
            if not given_argument:
                continue

            given_argument = given_argument[0]
            given_argument = given_argument.value if isinstance(given_argument, ObjectInitializerArgumentNamedAst) else given_argument.identifier
            if attribute.type != given_argument.infer_type(scope_handler):
                exception = SemanticError(f"Invalid type '{given_argument.infer_type(scope_handler)}' given to attribute '{attribute.name}':")
                exception.add_traceback(attribute.name.pos, f"Attribute '{attribute.name}' declared here with type '{attribute.type}'.")
                exception.add_traceback(given_argument.pos, f"Attribute '{attribute.name}' given value here with type '{given_argument.infer_type(scope_handler)}'.")
                raise exception

        # TODO : below
        # Check that each parent class has a value given in "sup=", if the parent class is stateful:
        # for parent_class in Seq(sup_classes).filter(lambda s: s.all_symbols(exclusive=True).filter(lambda s: isinstance(s, VariableSymbol)).length > 0):
        #     sup_argument = (Seq(self.arguments.arguments)
        #             .filter(lambda a: isinstance(a, ObjectInitializerArgumentNamedAst))
        #             .filter(lambda a: isinstance(a.identifier, TokenAst) and a.identifier.token.token_type == TokenType.KwSup))
        #
        #     # Check that there is a maximum of 1 "sup=" argument given:
        #     if sup_argument.length > 1:
        #         exception = SemanticError(f"Multiple 'sup=' arguments given in object initializer:")
        #         exception.add_traceback(sup_argument[0].pos, f"1st 'sup=' argument given here.")
        #         exception.add_traceback(sup_argument[1].pos, f"2nd 'sup=' argument given here.")
        #         raise exception
        #
        #     # Check that the "sup=" argument is of the correct type:
        #     sup_class_types = Seq(sup_classes).map(lambda s: s.type).value
        #     if sup_argument and sup_argument[0].value.infer_type(scope_handler) != CommonTypes.tuple(sup_class_types):
        #         exception = SemanticError(f"Invalid type '{sup_argument[0].value.infer_type(scope_handler)}' given to 'sup=':")
        #         exception.add_traceback(sup_argument[0].value.pos, f"'sup=' argument given here with type '{sup_argument[0].value.infer_type(scope_handler)}', instead of '{CommonTypes.tuple(sup_class_types)}'.")
        #         exception.add_traceback(self.pos, f"Object initializer declared here.")
        #         raise exception

        # TODO : generic arguments
        # TODO : check the memory status of the object initializer arguments

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, self.class_type


@dataclass
class ParenthesizedExpressionAst(Ast, SemanticAnalysis, TypeInfer):
    paren_l_token: TokenAst
    expression: ExpressionAst
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.paren_l_token.print(printer)}{self.expression.print(printer)}{self.paren_r_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.expression.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return self.expression.infer_type(scope_handler)


@dataclass
class PatternVariantTupleAst(Ast, SemanticAnalysis):
    variable: LocalVariableTupleAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variable.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    # def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
    #     tuple_element_types = kwargs.get("if-expression").condition.infer_type()
    #     if not isinstance(tuple_element_types, TypeTupleAst):
    #         exception = SemanticError(f"A tuple")
    #
    #     for i, element in enumerate(self.variable.items):
    #         match element:
    #             case LocalVariableTupleAst(): ...
    #             case LocalVariableDestructureAst(): ...
    #             case LocalVariableSingleAst():
    #                 let_statement = LetStatementUninitializedAst(
    #                     pos=element.pos,
    #                     let_keyword=TokenAst.dummy(TokenType.KwLet),
    #                     assign_to=element,
    #                     colon_token=TokenAst.dummy(TokenType.TkColon),
    #
    #                 )
    #
    #             case PatternVariantLiteralAst(): ...
    #             case _:
    #                 exception = SemanticError(f"Invalid pattern variant inside a tuple pattern:")
    #                 exception.add_traceback(element.pos, f"Pattern variant '{element}' found here.")
    #                 raise exception



@dataclass
class PatternVariantDestructureAst(Ast):
    variable: LocalVariableDestructureAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variable.print(printer)}"


@dataclass
class PatternVariantVariableAst(Ast, SemanticAnalysis):
    variable: LocalVariableSingleAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variable.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    # def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
    #     let_statement = LetStatementUninitializedAst(
    #         pos=self.variable.pos,
    #         let_keyword=TokenAst.dummy(TokenType.KwLet),
    #         assign_to=self.variable,
    #         colon_token=TokenAst.dummy(TokenType.TkColon),
    #         type_declaration=CommonTypes.void())
    #     let_statement.do_semantic_analysis(scope_handler, **kwargs)


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
class PatternBlockAst(Ast, SemanticAnalysis):
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

    def is_else_branch(self) -> bool:
        return isinstance(self.patterns[0], PatternVariantElseAst)

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        scope_handler.into_new_scope("<pattern-block>")
        if_expression = kwargs.get("if-expression")

        # Check that if the "if" expression has a comparison operator, that the branch doesn't.
        if if_expression.comp_operator and self.comp_operator:
            exception = SemanticError(f"Comparison operator '{self.comp_operator}' found in if-expression and pattern block:")
            exception.add_traceback(self.comp_operator.pos, f"1st Comparison operator '{self.comp_operator}' found here.")
            exception.add_traceback(if_expression.comp_operator.pos, f"2nd Comparison operator '{if_expression.comp_operator}' found here.")
            raise exception

        Seq(self.patterns).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))
        self.guard.do_semantic_analysis(scope_handler, **kwargs) if self.guard else None
        self.body.do_semantic_analysis(scope_handler, **kwargs)
        scope_handler.exit_cur_scope()


@dataclass
class PatternGuardAst(Ast, SemanticAnalysis):
    guard_token: TokenAst
    expression: ExpressionAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.guard_token.print(printer)}{self.expression.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.expression.do_semantic_analysis(scope_handler, **kwargs)

        expression_type = self.expression.infer_type(scope_handler)
        if expression_type != CommonTypes.bool():
            exception = SemanticError(f"Guard expression must be of type 'Bool':")
            exception.add_traceback(self.expression.pos, f"Expression '{self.expression}' has type '{expression_type}'.")
            raise exception


@dataclass
class PostfixExpressionAst(Ast, SemanticAnalysis, TypeInfer):
    lhs: ExpressionAst
    op: PostfixExpressionOperatorAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.lhs.print(printer)}{self.op.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        self.lhs.do_semantic_analysis(scope_handler, **kwargs)
        self.op.do_semantic_analysis(scope_handler, **(kwargs | {"postfix-lhs": self.lhs}))

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return self.op.infer_type(scope_handler, **(kwargs | {"postfix-lhs": self.lhs}))


@dataclass
class PostfixExpressionOperatorFunctionCallAst(Ast, SemanticAnalysis, TypeInfer):
    generic_arguments: Optional[GenericArgumentGroupAst]
    arguments: FunctionArgumentGroupAst
    fold_token: Optional[TokenAst]

    def __post_init__(self):
        self.generic_arguments = self.generic_arguments or GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.generic_arguments.print(printer)}" if self.generic_arguments else ""
        s += f"{self.arguments.print(printer)}"
        s += f"{self.fold_token.print(printer)}" if self.fold_token else ""
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> FunctionPrototypeAst:
        function_name = kwargs.get("postfix-lhs")

        # Check that the function being called exists with this overload.
        mock_function_object_name = TypeSingleAst(-1, [GenericIdentifierAst(-1, f"__MOCK_{function_name}", None)])
        mock_function_object_name.do_semantic_analysis(scope_handler, **kwargs)
        mock_function_object = scope_handler.current_scope.get_symbol(mock_function_object_name)

        # The only classes possibly superimposed over a __MOCK_ class are the Fun classes.
        mock_function_object_sup_scopes = scope_handler.current_scope.get_symbol(mock_function_object_name).associated_scope.sup_scopes
        function_overloads = Seq(mock_function_object_sup_scopes).map(lambda s: s[1].body.members[0])

        # Check each function overload if it valid for this function call
        # TODO : generics, named arguments
        # TODO : variadic parameter, optional parameters
        self.generic_arguments.do_semantic_analysis(scope_handler, **kwargs)
        self.arguments.do_semantic_analysis(scope_handler, **kwargs)
        argument_types = Seq(self.arguments.arguments).map(lambda a: a.value.infer_type(scope_handler))

        error_message = f""
        for function_overload in function_overloads:
            parameter_types = Seq(function_overload.parameters.parameters).map(lambda p: (p.convention, p.type_declaration))
            error_message += (
                f"\n\t{function_name}"
                f"({", ".join(parameter_types.map(lambda t: f"{t[0].default()}{t[1]}").value)}) "
                f"-> {function_overload.return_type}")

            if argument_types.length != parameter_types.length:
                continue

            if argument_types.zip(parameter_types).any(lambda t: t[0][0] != type(t[0][1]) and t[0][1] != t[1][1]):
                continue

            return function_overload

        exception = SemanticError(f"Invalid function call:")
        exception.add_traceback(self.pos, (
            f"Function call"
            f" '({", ".join(argument_types.map(lambda t: f"{t[0].default()}{t[1]}").value)})'"
            f" found here."))
        exception.add_footer(f"Valid overloads are:{error_message}")
        raise exception

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        return ConventionMovAst, self.do_semantic_analysis(scope_handler, **kwargs).return_type


@dataclass
class PostfixExpressionOperatorMemberAccessAst(Ast, SemanticAnalysis):
    dot_token: TokenAst
    identifier: PostfixMemberPartAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.dot_token.print(printer)}{self.identifier.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        lhs = kwargs.get("postfix-lhs")
        lhs_type_scope = scope_handler.current_scope.get_symbol(lhs.infer_type(scope_handler)).associated_scope

        # Check that, for numeric access, the LHS is a tuple type with enough elements in it.
        if isinstance(self.identifier, LiteralNumberBase10Ast):
            if not isinstance(lhs.infer_type().without_generics(), CommonTypes.tuple([])):
                exception = SemanticError(f"Numeric member access requires a tuple type:")
                exception.add_traceback(lhs.pos, f"Type '{lhs.infer_type()}' found here.")
                exception.add_traceback(self.identifier.pos, f"Numeric member access found here.")
                raise exception

            if int(self.identifier.number.token.token_metadata) >= len(lhs.infer_type().parts[-1].generic_arguments.arguments):
                exception = SemanticError(f"Numeric member access out of bounds:")
                exception.add_traceback(lhs.pos, f"Type '{lhs.infer_type()}' found here, with {len(lhs.infer_type().parts[-1].gemeric_arguments.arguments)} elements.")
                exception.add_traceback(self.identifier.pos, f"Numeric member access found here to element {self.identifier.number.token.token_metadata}.")
                raise exception

        # Check that, for attribute access, the attribute exists on the type being accessed.
        elif isinstance(self.identifier, IdentifierAst) and not lhs_type_scope.has_symbol(self.identifier):
            exception = SemanticError(f"Undefined attribute '{self.identifier.value}':")
            exception.add_traceback(lhs.pos, f"Type '{lhs.infer_type(scope_handler)}' found here.")
            exception.add_traceback(self.identifier.pos, f"Attribute '{self.identifier.value}' accessed here.")
            raise exception

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        lhs = kwargs.get("postfix-lhs")
        lhs_type_scope = scope_handler.current_scope.get_symbol(lhs.infer_type(scope_handler)).associated_scope

        #
        if isinstance(self.identifier, LiteralNumberBase10Ast):
            return ConventionMovAst, lhs.infer_type().parts[-1].generic_arguments.arguments[int(self.identifier.number.token.token_metadata)]

        #
        elif isinstance(self.identifier, IdentifierAst):
            return ConventionMovAst, lhs_type_scope.get_symbol(self.identifier).type


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
class ProgramAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalysis):
    module: ModulePrototypeAst
    eof_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.module.print(printer)}"

    def pre_process(self, context: ModulePrototypeAst) -> None:
        Seq(self.module.body.members).for_each(lambda m: m.pre_process(context))
        self.module.body.members = Seq(self.module.body.members).filter(lambda m: not isinstance(m, FunctionPrototypeAst)).value

    def generate(self, s: ScopeHandler) -> None:
        Seq(self.module.body.members).for_each(lambda m: m.generate(s))

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        self.module.do_semantic_analysis(scope_handler, **kwargs)


@dataclass
class ResidualInnerScopeAst(Ast):
    else_keyword: TokenAst
    body: InnerScopeAst[StatementAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.else_keyword.print(printer)}{self.body.print(printer)}"


@dataclass
class ReturnStatementAst(Ast, SemanticAnalysis):
    return_keyword: TokenAst
    expression: Optional[ExpressionAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.return_keyword.print(printer)}"
        s += f"{self.expression.print(printer)}" if self.expression else ""
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        self.expression.do_semantic_analysis(scope_handler, **kwargs) if self.expression else None
        target_return_type = kwargs.get("target-return-type")

        match self.expression:
            case IdentifierAst(): sym = scope_handler.current_scope.get_symbol(self.expression)
            case PostfixExpressionAst(): sym = scope_handler.current_scope.get_symbol(self.expression.lhs)
            case _: sym = None

        # if sym and sym.memory_info.ast_consumed:
        #     exception = SemanticError(f"Returning uninitialized variable:")
        #     exception.add_traceback(sym.memory_info.ast_consumed.pos, f"Variable '{self.expression}' uninitialized/moved here.")
        #     exception.add_traceback(self.pos, f"Variable '{self.expression}' returned here.")
        #     raise exception

        # if sym and sym.memory_info.ast_borrow:
        #     exception = SemanticError(f"Returning borrowed variable:")
        #     exception.add_traceback(sym.memory_info.ast_borrow.pos, f"Variable '{self.expression}' borrowed here.")
        #     exception.add_traceback(self.pos, f"Variable '{self.expression}' returned here.")
        #     raise exception

        # if sym and sym.memory_info.ast_partial_moves:
        #     exception = SemanticError(f"Returning partially moved variable:")
        #     exception.add_traceback(sym.memory_info.ast_partial_moves[0].pos, f"Variable '{self.expression}' partially moved here.")
        #     exception.add_traceback(self.pos, f"Variable '{self.expression}' returned here.")
        #     raise exception

        found_return_type = self.expression.infer_type(scope_handler)
        if found_return_type != (ConventionMovAst, target_return_type):
            exception = SemanticError(f"Returning variable of incorrect type:")
            exception.add_traceback(target_return_type.pos, f"Function has return type '{target_return_type}'.")
            exception.add_traceback(self.pos, f"Variable '{self.expression}' returned here is type '{found_return_type[0].default()}{found_return_type[1]}'.")
            raise exception


@dataclass
class SupMethodPrototypeAst(FunctionPrototypeAst):
    ...


@dataclass
class SupPrototypeNormalAst(Ast, PreProcessor, SymbolGenerator, SemanticAnalysis):
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
        self.body.members = Seq(self.body.members).filter(lambda m: not isinstance(m, FunctionPrototypeAst)).value

    def generate(self, s: ScopeHandler) -> None:
        s.into_new_scope(IdentifierAst(-1, self.identifier.parts[-1].value + "#SUP"))
        Seq(self.body.members).for_each(lambda m: m.generate(s))
        Seq(self.generic_parameters.parameters).for_each(lambda p: s.current_scope.add_symbol(TypeSymbol(p.identifier, None)))
        s.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        scope_handler.move_to_next_scope()
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        # self.where_block.do_semantic_analysis(s)
        self.identifier.do_semantic_analysis(scope_handler, **kwargs)
        Seq(self.body.members).for_each(lambda m: m.do_semantic_analysis(scope_handler, **kwargs))
        scope_handler.exit_cur_scope()


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

    def pre_process(self, context: ModulePrototypeAst) -> None:
        if self.super_class.parts[-1].value in ["FunRef", "FunMut", "FunOne"]:
            return
        super().pre_process(context)

    def generate(self, s: ScopeHandler) -> None:
        s.into_new_scope(IdentifierAst(-1, self.identifier.parts[-1].value + "#SUP"))
        cls_scope = s.current_scope.get_symbol(self.identifier).associated_scope
        cls_scope._sup_scopes.append((s.current_scope, self))
        Seq(self.body.members).for_each(lambda m: m.generate(s))
        Seq(self.generic_parameters.parameters).for_each(lambda p: s.current_scope.add_symbol(TypeSymbol(p.identifier, None)))
        s.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        scope_handler.move_to_next_scope()
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        # self.where_block.do_semantic_analysis(s)
        self.super_class.do_semantic_analysis(scope_handler, **kwargs)
        self.identifier.do_semantic_analysis(scope_handler, **kwargs)
        Seq(self.body.members).for_each(lambda m: m.do_semantic_analysis(scope_handler, **kwargs))
        scope_handler.exit_cur_scope()

        # TODO : check there are no direct duplicate sup super-classes
        # TODO : check overriden typedefs & methods appear on super-class
        # TODO : check there are no duplicate / overlapping overloads of methods for this sup-block
        #   - At this point, all sup-blocks are discovered, so we can check for duplicate / overlapping overloads.
        #   - If in this function, it'll happen for every sup-block -- only needs to happen once though (cls block?)


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
class TypeSingleAst(Ast, SemanticAnalysis):
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

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        sym = scope_handler.current_scope.get_symbol(self)

        # Check that the type exists in the symbol table.
        if not sym:
            exception = SemanticError(f"Type '{self}' is not defined:")
            exception.add_traceback(self.pos, f"Type '{self}' used here.")
            raise exception

        # Check that the number of generic arguments given to the type is <= the number of generic parameters.
        # TODO : bypass this if the final generic parameter is variadic.
        if sym.type and len(self.parts[-1].generic_arguments.arguments) > len(sym.type.generic_parameters.parameters):
            exception = SemanticError(f"Too many generic arguments given to type '{self.without_generics()}':")
            exception.add_traceback(sym.type.pos, f"Type {self.without_generics()} declared here.")
            exception.add_traceback(self.pos, f"Type '{self}' used here.")
            raise exception

        # TODO : check if < required amount of generic parameters have been given too.

    def without_generics(self) -> TypeSingleAst:
        parts = []
        for part in self.parts:
            parts.append(GenericIdentifierAst(-1, part.value, GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))) if isinstance(part, GenericIdentifierAst) else part)
        return TypeSingleAst(self.pos, parts)

    def __eq__(self, other):
        return isinstance(other, TypeSingleAst) and self.parts == other.parts

    def __hash__(self):
        return int.from_bytes(hashlib.md5("".join([str(p) for p in self.parts]).encode()).digest())

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
class WithExpressionAliasAst(Ast, SemanticAnalysis):
    variable: LocalVariableAst
    assign_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        return f"{self.variable.print(printer)}{self.assign_token.print(printer)}"

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        let_statement = LetStatementInitializedAst(
            pos=self.variable.pos,
            let_keyword=TokenAst.dummy(TokenType.KwLet),
            assign_to=self.variable,
            assign_token=self.assign_token,
            value=kwargs.get("with-expression-value"),
            residual=None)
        let_statement.do_semantic_analysis(scope_handler, **kwargs)


@dataclass
class WithExpressionAst(Ast, SemanticAnalysis, TypeInfer):
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

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        scope_handler.into_new_scope("<with-expression>")

        # Check that the type of object used in the "with" expression superimposes Ctx
        self.expression.do_semantic_analysis(scope_handler, **kwargs)
        object_type = self.expression.infer_type(scope_handler)
        object_type_sup_types = scope_handler.current_scope.get_symbol(object_type).associated_scope.sup_scopes
        if CommonTypes.ctx() not in object_type_sup_types:
            exception = SemanticError(f"Type '{object_type}' does not superimpose Ctx:")
            exception.add_traceback(self.expression.pos, f"Expression '{self.expression}' has type '{object_type}'.")
            raise exception

        # Create the symbol for the alias
        if self.alias:
            self.alias.do_semantic_analysis(scope_handler, **(kwargs | {"with-expression-value": self.expression}))

        self.body.do_semantic_analysis(scope_handler, **kwargs)
        scope_handler.exit_cur_scope()


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

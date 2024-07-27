from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass
from typing import Dict, Type, Optional

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler, Scope
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.Utils.Sequence import Seq


def infer_generics_types(
        ast: Ast,
        generic_parameters: list["TypeAst"],
        explicit_generic_arguments: Dict["TypeAst", "TypeAst"],
        infer_from: Dict["IdentifierAst", "TypeAst"],
        map_to: Dict["IdentifierAst", "TypeAst"],
        scope_handler: ScopeHandler,
        supress_missing: bool = False) -> Dict["TypeAst", "TypeAst"]:

    from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors

    # print("#" * 100)
    # print(ast)
    # print("Generic parameters:", [str(generic) for generic in generic_parameters])
    # print("Explicit generic arguments:", {str(key): str(value) for key, value in explicit_generic_arguments.items()})
    # print("Infer from:", {str(key): str(value) for key, value in infer_from.items()})
    # print("Map to:", {str(key): str(value) for key, value in map_to.items()})

    """
    For the class:

    cls Point[T, U, V] {
        x: T
        y: U
        z: V
    }

    let p = Point(x=1, y="hello", z=False)

    the arguments:
        generic_parameters: [T, U, V]
        explicit_generic_arguments: {}
        infer_from: {x: 1, y: "hello", z: False}
        map_to: {x: T, y: U, z: V}
    """

    # Infer all possible generic arguments.
    inferred_generic_arguments = {}
    for identifier, value in infer_from.items():
        if identifier in map_to.keys() and map_to[identifier] in generic_parameters and map_to[identifier] not in inferred_generic_arguments:
            inferred_generic_arguments[map_to[identifier]] = value

    # Check no inferred generic arguments have conflicting inferred types.
    for inferred_generic_argument in inferred_generic_arguments.keys():
        if list(map_to.values()).count(inferred_generic_argument) > 1:
            inferred = [infer_from[identifier] for identifier, generic in map_to.items() if map_to[identifier] in generic_parameters and generic == inferred_generic_argument]
            for i in inferred:
                for j in inferred:
                    if not i.symbolic_eq(j, scope_handler.current_scope):
                        raise SemanticErrors.CONFLICTING_GENERIC_INFERENCE(inferred_generic_argument, inferred[0], inferred[1])

    # Check no inferred generic arguments are already explicitly defined.
    for inferred_generic_argument in inferred_generic_arguments.keys():
        if inferred_generic_argument in explicit_generic_arguments:  # and explicit_generic_arguments[inferred_generic_argument].symbolic_eq(inferred_generic_arguments[inferred_generic_argument]):
            raise SemanticErrors.GENERIC_INFERRABLE(inferred_generic_argument, explicit_generic_arguments[inferred_generic_argument], inferred_generic_arguments[inferred_generic_argument])

    # Check all generic parameters have been inferred or explicitly defined.
    for generic_parameter in generic_parameters:
        if generic_parameter not in explicit_generic_arguments and generic_parameter not in inferred_generic_arguments:
            if not supress_missing:
                raise SemanticErrors.MISSING_GENERIC_ARGUMENT(ast, generic_parameter)

    # Return a union of the inferred and explicit generic arguments.
    return inferred_generic_arguments | explicit_generic_arguments


def ensure_memory_integrity(
        entire_ast: Ast, value_ast: Ast, move_ast: Ast, scope_handler: ScopeHandler,
        check_move: bool = True, check_partial_move: bool = True, check_move_from_borrowed_context: bool = True,
        mark_symbols: bool = True) -> None:

    from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, PostfixExpressionAst
    from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors

    # Get the symbol, for Identifiers and PostfixExpressions. If the value isn't either of these, then the memory rules
    # don't apply, so all the checks are skipped.
    symbol = scope_handler.current_scope.get_outermost_variable_symbol(value_ast)
    if not symbol:
        return

    # 1. Check the symbol has not been consumed by another move. This prevents double moves or using uninitialized values.
    if check_move and isinstance(value_ast, IdentifierAst) and symbol.memory_info.ast_consumed:
        raise SemanticErrors.USING_NON_INITIALIZED_VALUE(value_ast, symbol)

    # 2. Check the symbol does not have any partial move.
    if check_partial_move and isinstance(value_ast, IdentifierAst) and symbol.memory_info.ast_partial_moves:
        raise SemanticErrors.USING_PARTIAL_MOVED_VALUE(value_ast, symbol)

    # 3. Check there are no overlapping partial moves for postfix expressions.
    if check_partial_move and isinstance(value_ast, PostfixExpressionAst) and symbol.memory_info.ast_partial_moves:
        for partial_move in symbol.memory_info.ast_partial_moves:
            if str(partial_move).startswith(str(value_ast)):
                raise SemanticErrors.USING_PARTIAL_MOVED_VALUE(value_ast, symbol)

    # 4. Check the symbol is not being moved from a borrowed context.
    if check_move_from_borrowed_context and isinstance(value_ast, PostfixExpressionAst) and symbol.memory_info.is_borrow:
        raise SemanticErrors.MOVING_FROM_BORROWED_CONTEXT(value_ast, move_ast, symbol)

    # 5. Mark the symbol as consumed or partially moved.
    if mark_symbols:
        match value_ast:
            case IdentifierAst():
                symbol.memory_info.ast_consumed = move_ast
                symbol.memory_info.ast_initialized = None
            case PostfixExpressionAst():
                symbol.memory_info.ast_partial_moves.append(value_ast)


def convert_generic_arguments_to_named(
        generic_arguments: Seq["GenericArgumentAst"],
        generic_parameters: Seq["GenericParameterAst"]) -> Seq["GenericArgumentAst"]:

    from SPPCompiler.SemanticAnalysis.ASTs import (
        GenericArgumentNamedAst, GenericArgumentNormalAst, GenericParameterOptionalAst, GenericParameterVariadicAst,
        TokenAst)

    is_variadic = generic_parameters and isinstance(Seq(generic_parameters)[-1], GenericParameterVariadicAst)

    # Remove the named generic arguments from the list of generic parameter identifiers.
    generic_parameter_identifiers = Seq(generic_parameters.value.copy()).map(lambda p: p.identifier)
    for generic_argument in generic_arguments.filter_to_type(GenericArgumentNamedAst):
        generic_parameter_identifiers.remove(generic_argument.identifier)

    # Loop over every unnamed generic argument in the list.
    for i, generic_argument in generic_arguments.filter_to_type(GenericArgumentNormalAst).enumerate():

        # Check if the final generic parameter is variadic; if it is, then run separate steps.
        if generic_parameter_identifiers.length == 1 and is_variadic:
            final_generic_arguments = generic_arguments.filter_to_type(GenericArgumentNormalAst)[i:].map(lambda g: g.type)
            generic_parameter_identifier = generic_parameter_identifiers.pop(0).parts[-1].to_identifier()
            new_argument = GenericArgumentNamedAst(generic_argument.pos, generic_parameter_identifier, TokenAst.dummy(TokenType.TkAssign), CommonTypes.tuple(final_generic_arguments))
            generic_arguments.replace(generic_argument, new_argument)  # todo: ?
            break

        # For a normal generic parameter, assign the next generic parameter identifier to the generic argument.
        else:
            generic_parameter_identifier = generic_parameter_identifiers.pop(0).parts[-1].to_identifier()
            new_argument = GenericArgumentNamedAst(generic_argument.pos, generic_parameter_identifier, TokenAst.dummy(TokenType.TkAssign), generic_argument.type)
            generic_arguments.replace(generic_argument, new_argument)

    # Add default values for any remaining generic parameters.
    for generic_parameter in generic_parameters:
        if isinstance(generic_parameter, GenericParameterOptionalAst) and generic_parameter.identifier not in generic_arguments.map(lambda a: a.identifier):
            new_argument = GenericArgumentNamedAst(generic_parameter.pos, generic_parameter.identifier.parts[-1].to_identifier(), TokenAst.dummy(TokenType.TkAssign), generic_parameter.default_value)
            generic_arguments.append(new_argument)

    return generic_arguments


def convert_function_arguments_to_named(
        arguments: Seq["FunctionArgumentAst"],
        parameters: Seq["FunctionParameterAst"]) -> Seq["FunctionArgumentAst"]:

    from SPPCompiler.SemanticAnalysis.ASTs import (
        FunctionArgumentNamedAst, FunctionArgumentNormalAst, TokenAst, TupleLiteralAst, FunctionParameterVariadicAst)

    is_variadic = parameters and isinstance(Seq(parameters)[-1], FunctionParameterVariadicAst)

    # Remove the named arguments from the list of parameter identifiers.
    parameter_identifiers = Seq(parameters.value.copy()).map(lambda p: p.identifier_for_param())
    for argument in arguments.filter_to_type(FunctionArgumentNamedAst):
        parameter_identifiers.remove(argument.identifier)

    # Loop over every unnamed argument in the list.
    for j, argument in arguments.filter_to_type(FunctionArgumentNormalAst).enumerate():

        # Check if the final parameter is variadic; if it is, then run separate steps.
        if parameter_identifiers.length == 1 and is_variadic:

            # Value is a tuple of the remaining arguments.
            final_arguments = TupleLiteralAst(argument.pos, TokenAst.dummy(TokenType.TkParenL), arguments[j:].map(lambda a: a.value).value, TokenAst.dummy(TokenType.TkParenR))
            parameter_identifier = parameter_identifiers[0]  # todo: make .pop(0)?
            new_argument = FunctionArgumentNamedAst(argument.pos, parameter_identifier, TokenAst.dummy(TokenType.TkAssign), argument.convention, None, final_arguments)
            arguments = arguments[:j]
            arguments.append(new_argument)
            break

        # For a normal parameter, assign the next parameter identifier to the argument.
        else:
            parameter_identifier = parameter_identifiers.pop(0)
            new_argument = FunctionArgumentNamedAst(argument.pos, parameter_identifier, TokenAst.dummy(TokenType.TkAssign), argument.convention, None, argument.value)
            arguments.replace(argument, new_argument)

    return arguments


class TypeInfer(ABC):
    @abstractmethod
    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        pass


@dataclass(kw_only=True)
class InferredType:
    convention: Type["ConventionAst"]
    type: "TypeAst"

    def symbolic_eq(self, other: InferredType, scope: Scope, that_scope: Optional[Scope] = None) -> bool:
        return self.convention == other.convention and self.type.symbolic_eq(other.type, scope, that_scope)

    def __str__(self):
        return f"{self.convention}{self.type}"


__all__ = [
    "TypeInfer",
    "InferredType",
    "infer_generics_types",
    "ensure_memory_integrity",
    "convert_generic_arguments_to_named",
    "convert_function_arguments_to_named"
]

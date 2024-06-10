from __future__ import annotations
from typing import Dict

from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, TypeAst
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast


def infer_generics_types(
        generic_parameters: list[TypeAst],
        explicit_generic_arguments: Dict[TypeAst, TypeAst],
        infer_from: Dict[IdentifierAst, TypeAst],
        map_to: Dict[IdentifierAst, TypeAst],
        scope_handler: ScopeHandler) -> Dict[TypeAst, TypeAst]:

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
            raise SemanticErrors.MISSING_GENERIC_ARGUMENT(generic_parameter)

    # Return a union of the inferred and explicit generic arguments.
    return inferred_generic_arguments | explicit_generic_arguments


def ensure_memory_integrity(
        entire_ast: Ast, value_ast: Ast, move_ast: Ast, scope_handler: ScopeHandler,
        check_move: bool = True, check_partial_move: bool = True, check_move_from_borrowed_context: bool = True,
        mark_symbols: bool = True) -> None:

    from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, PostfixExpressionAst

    symbol = scope_handler.current_scope.get_outermost_variable_symbol(value_ast)

    # 1. Check the symbol has not been consumed by another move. This prevents double moves or using uninitialized values.
    if check_move and isinstance(value_ast, IdentifierAst) and symbol and symbol.memory_info.ast_consumed:
        raise SemanticErrors.USING_NON_INITIALIZED_VALUE(value_ast, symbol)

    # 2. Check the symbol does not have any partial move.
    if check_partial_move and isinstance(value_ast, IdentifierAst) and symbol and symbol.memory_info.ast_partial_moves:
        raise SemanticErrors.USING_PARTIAL_MOVED_VALUE(value_ast, symbol)

    # 3. Check there are no overlapping partial moves for postfix expressions.
    if check_partial_move and isinstance(value_ast, PostfixExpressionAst) and symbol and symbol.memory_info.ast_partial_moves:
        for partial_move in symbol.memory_info.ast_partial_moves:
            if str(partial_move).startswith(str(value_ast)):
                raise SemanticErrors.USING_PARTIAL_MOVED_VALUE(value_ast, symbol)

    # 4. Check the symbol is not being moved from a borrowed context.
    if check_move_from_borrowed_context and isinstance(value_ast, PostfixExpressionAst) and symbol.memory_info.is_borrow:
        raise SemanticErrors.MOVING_FROM_BORROWED_CONTEXT(value_ast, move_ast, symbol)

    # 5. Mark the symbol as consumed or partially moved.
    if mark_symbols:
        match value_ast:
            case IdentifierAst(): symbol.memory_info.ast_consumed = move_ast
            case PostfixExpressionAst(): symbol.memory_info.ast_partial_moves.append(value_ast)


__all__ = ["infer_generics_types"]

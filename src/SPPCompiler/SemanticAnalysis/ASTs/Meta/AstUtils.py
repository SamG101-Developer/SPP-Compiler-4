from __future__ import annotations

from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple, Type

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler, Scope
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol
from SPPCompiler.Utils.Sequence import Seq


def infer_generics_types(
        ast: "TypeAst",
        generic_parameters: List["TypeAst"],
        explicit_generic_arguments: Dict["TypeAst", "TypeAst"],
        infer_from: Dict["IdentifierAst", "TypeAst"],
        map_to: Dict["IdentifierAst", "TypeAst"],
        scope_handler: ScopeHandler) -> Seq["GenericArgumentAst"]:

    from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
    from SPPCompiler.SemanticAnalysis.ASTs import GenericArgumentNamedAst, TokenAst, TypeAst

    if isinstance(ast, TypeAst) and ast.without_generics() == CommonTypes.tuple([]):
        all_generic_arguments = [GenericArgumentNamedAst(
            pos=-1,
            raw_identifier=identifier.types[-1].to_identifier(),
            assignment_token=TokenAst.dummy(TokenType.TkAssign),
            type=type) for identifier, type in explicit_generic_arguments.items()]
        return Seq(all_generic_arguments)

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
        if inferred_generic_argument in explicit_generic_arguments:
            raise SemanticErrors.GENERIC_INFERRABLE(inferred_generic_argument, explicit_generic_arguments[inferred_generic_argument], inferred_generic_arguments[inferred_generic_argument])

    # Check all generic parameters have been inferred or explicitly defined.
    for generic_parameter in generic_parameters:
        if generic_parameter not in explicit_generic_arguments | inferred_generic_arguments:
            raise SemanticErrors.MISSING_GENERIC_ARGUMENT(ast, generic_parameter)

    # Return a union of the inferred and explicit generic arguments.
    all_generic_arguments = inferred_generic_arguments | explicit_generic_arguments

    all_generic_arguments = [GenericArgumentNamedAst(
        pos=-1,
        raw_identifier=identifier.types[-1].to_identifier(),
        assignment_token=TokenAst.dummy(TokenType.TkAssign),
        type=type) for identifier, type in all_generic_arguments.items()]

    return Seq(all_generic_arguments)


def ensure_memory_integrity(
        entire_ast: Ast, value_ast: Ast, move_ast: Ast, scope_handler: ScopeHandler,
        check_move: bool = True, check_partial_move: bool = True, check_move_from_borrowed_context: bool = True,
        check_pinned_move: bool = True, mark_symbols: bool = True) -> None:

    from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, PostfixExpressionAst
    from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors

    # Get the symbol, for Identifiers and PostfixExpressions. If the value isn't either of these, then the memory rules
    # don't apply, so all the checks are skipped.
    symbol = scope_handler.current_scope.get_outermost_variable_symbol(value_ast)
    if not symbol:
        return

    # 0. Check for inconsistent memory status (from case block branches).
    if symbol.memory_info.ast_initialized == "Inconsistent":
        raise SemanticErrors.INCONSISTENT_MEMORY_INIT_STATUS(value_ast)
    if symbol.memory_info.ast_pins == "Inconsistent":
        raise SemanticErrors.INCONSISTENT_MEMORY_PIN_STATUS(value_ast)

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

    # 5. Check the symbol is not pinned.
    if check_pinned_move:
        for existing_pin in symbol.memory_info.ast_pins:
            # Global constants need namespace removed here.
            str_value_ast = str(value_ast).split("::")[-1]
            if str_value_ast.startswith(str(existing_pin)) or str(existing_pin).startswith(str_value_ast):
                raise SemanticErrors.MOVING_PINNED_VALUE(value_ast, existing_pin)

    # 6. Mark the symbol as consumed or partially moved.
    if mark_symbols:
        match value_ast:
            case IdentifierAst():
                symbol.memory_info.ast_consumed = move_ast
                symbol.memory_info.ast_initialized = None
            case PostfixExpressionAst():
                symbol.memory_info.ast_partial_moves.append(value_ast)


def convert_generic_arguments_to_named(
        generic_arguments: Seq["GenericArgumentAst"],
        generic_parameters: Seq["GenericParameterAst"],
        generic_parameters_scope: Scope) -> Seq["GenericArgumentAst"]:

    from SPPCompiler.SemanticAnalysis.ASTs import (
        GenericArgumentNamedAst, GenericArgumentNormalAst, GenericParameterOptionalAst, GenericParameterVariadicAst,
        TokenAst)

    is_variadic = generic_parameters and isinstance(Seq(generic_parameters)[-1], GenericParameterVariadicAst)

    # Remove the named generic arguments from the list of generic parameter identifiers.
    generic_parameter_identifiers = Seq(generic_parameters.list()).map(lambda p: p.identifier)
    for generic_argument in generic_arguments.filter_to_type(GenericArgumentNamedAst):
        generic_parameter_identifiers.remove(generic_argument.identifier)

    # Loop over every unnamed generic argument in the list.
    for i, generic_argument in generic_arguments.filter_to_type(GenericArgumentNormalAst).enumerate():
        # Check if the final generic parameter is variadic; if it is, then run separate steps.
        if generic_parameter_identifiers.length == 1 and is_variadic:
            final_generic_arguments = generic_arguments.filter_to_type(GenericArgumentNormalAst)[i:].map(lambda g: g.type)
            generic_parameter_identifier = generic_parameter_identifiers.pop(0).types[-1].to_identifier()
            new_argument = GenericArgumentNamedAst(generic_argument.pos, generic_parameter_identifier, TokenAst.dummy(TokenType.TkAssign), CommonTypes.tuple(final_generic_arguments))
            generic_arguments[i:] = [new_argument]
            break

        # For a normal generic parameter, assign the next generic parameter identifier to the generic argument.
        else:
            generic_parameter_identifier = generic_parameter_identifiers.pop(0).types[-1].to_identifier()
            new_argument = GenericArgumentNamedAst(generic_argument.pos, generic_parameter_identifier, TokenAst.dummy(TokenType.TkAssign), generic_argument.type)
            generic_arguments.replace(generic_argument, new_argument)

    # Add default values for any remaining generic parameters.
    for generic_parameter in generic_parameters.filter_to_type(GenericParameterOptionalAst):
        if generic_parameter.identifier not in generic_arguments.map(lambda a: a.identifier):
            generic_default_name = generic_parameter.identifier.types[-1].to_identifier()
            generic_default_type = generic_parameters_scope.get_symbol(generic_parameter.default_value).fq_type
            new_argument = GenericArgumentNamedAst(generic_parameter.pos, generic_default_name, TokenAst.dummy(TokenType.TkAssign), generic_default_type)
            generic_arguments.append(new_argument)

    return generic_arguments


def convert_function_arguments_to_named(
        arguments: Seq["FunctionArgumentAst"],
        parameters: Seq["FunctionParameterAst"]) -> Seq["FunctionArgumentAst"]:

    from SPPCompiler.SemanticAnalysis.ASTs import (
        FunctionArgumentNamedAst, FunctionArgumentNormalAst, TokenAst, TupleLiteralAst, FunctionParameterVariadicAst)

    is_variadic = parameters and isinstance(Seq(parameters)[-1], FunctionParameterVariadicAst)

    # Remove the named arguments from the list of parameter identifiers.
    parameter_identifiers = Seq(parameters.list()).map(lambda p: p.identifier_for_param())
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
            new_argument = FunctionArgumentNamedAst(argument.pos, parameter_identifier, TokenAst.dummy(TokenType.TkAssign), argument.convention, argument.value)
            arguments.replace(argument, new_argument)

    return arguments


def get_all_function_scopes(type_scope: Scope, identifier: "IdentifierAst") -> Seq[Tuple[Scope, "SupPrototypeNormalAst"]]:
    from SPPCompiler.LexicalAnalysis.Lexer import Lexer
    from SPPCompiler.SyntacticAnalysis.Parser import Parser

    # Create the mock identifier and get the scopes of the mock method classes.
    converted_identifier = Parser(Lexer(f"MOCK_{identifier}").lex(), "").parse_generic_identifier().parse_once()
    mock_scopes = Seq(type_scope.get_all_symbols(converted_identifier)).map(lambda sym: sym.associated_scope).list()
    mock_scopes.sort(key=lambda scope: type_scope.depth_to(scope.parent), reverse=True)

    # Remove overloads from further away classes that are overridden by closer classes.
    # func_scopes_2 = {scope: scope.sup_scopes for scope in mock_scopes}
    # for scope, sup_scopes in func_scopes_2.items():
    #     for sup_scope in sup_scopes:
    #         for following_scope, following_sup_scopes in func_scopes_2.items():
    #             for following_sup_scope in following_sup_scopes:
    #                 if type_scope.depth_to(scope.parent) < type_scope.depth_to(following_scope.parent):
    #                     print("Potential override:", sup_scope[0], following_sup_scope[0])
    #
    #                     this_member = sup_scope[1]
    #                     that_member = following_sup_scope[1]
    #                     # super_class_scope =
    #
    #                     if matching_function_types(this_member, that_member, scope, following_scope):
    #                         print("\t=> Overridden:", sup_scope[0], following_sup_scope[0])

    # for scope, sup_scopes in func_scopes_2.items():
    #     print(f"{scope}")
    #     for sup_scope in sup_scopes:
    #         print(f"\t{sup_scope[0]}")

    func_scopes = Seq([mock_scope.sup_scopes for mock_scope in mock_scopes]).flat()

    # Todo: just select the last one in the caller of this function?

    # Directly overridden functions must be removed from the available scopes.
    # print("-" * 100)
    # print(type_scope.name)
    # for mock_scope in mock_scopes:
    #     print(mock_scope.parent, type_scope.depth_to(mock_scope.parent))

    return func_scopes


def matching_function_types(this_member, that_member, current_scope: Scope, super_class_scope: Scope) -> bool:
    this_member_has_self_parameter = this_member.body.members[0].parameters.get_self() is not None
    that_member_has_self_parameter = that_member.body.members[0].parameters.get_self() is not None
    t1 = this_member.super_class.types[-1].generic_arguments.arguments[-1].type.types[-1].generic_arguments.arguments[this_member_has_self_parameter:]
    t2 = that_member.super_class.types[-1].generic_arguments.arguments[-1].type.types[-1].generic_arguments.arguments[that_member_has_self_parameter:]
    return all(tt1.type.symbolic_eq(tt2.type, current_scope, super_class_scope) for tt1, tt2 in zip(t1, t2))


def get_owner_type_of_sup_block(identifier: "IdentifierAst", scope_handler: ScopeHandler) -> Optional[TypeSymbol]:
    self_symbol = None
    if identifier.types[-1].value.startswith("MOCK_"):
        if isinstance(scope_handler.current_scope.parent.name, str) and "#" in scope_handler.current_scope.parent.name:
            class_type = scope_handler.current_scope.parent.name.split("#")[0]
            from SPPCompiler.SyntacticAnalysis.Parser import Parser
            from SPPCompiler.LexicalAnalysis.Lexer import Lexer
            class_type = Parser(Lexer(class_type).lex(), "").parse_type().parse_once()
            class_type.do_semantic_analysis(scope_handler)

            self_symbol = scope_handler.current_scope.get_symbol(class_type)

    return self_symbol


class TypeInfer:
    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        pass


@dataclass(kw_only=True)
class InferredType:
    convention: Type["ConventionAst"]
    type: "TypeAst"

    def symbolic_eq(self, other: InferredType, scope: Scope, that_scope: Optional[Scope] = None) -> bool:
        return self.convention == other.convention and self.type.symbolic_eq(other.type, scope, that_scope)

    def __str__(self):
        return f"{self.convention.dummy()}{self.type}"

    def __hash__(self):
        return hash((self.convention, self.type))


__all__ = [
    "TypeInfer",
    "InferredType",
    "infer_generics_types",
    "ensure_memory_integrity",
    "convert_generic_arguments_to_named",
    "convert_function_arguments_to_named"
]

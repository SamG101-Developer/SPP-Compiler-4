from __future__ import annotations

import copy, operator
from dataclasses import dataclass
from fastenum import Enum
from typing import Dict, List, Optional, Tuple, Type

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler, Scope
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol, VariableSymbol
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

    from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, PostfixExpressionAst, SupPrototypeInheritanceAst
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

    # 1. Check the symbol has not been consumed by another move (prevents double moves / uninitialized use).
    if check_move and isinstance(value_ast, IdentifierAst) and symbol.memory_info.ast_consumed:
        raise SemanticErrors.USING_NON_INITIALIZED_VALUE(value_ast, symbol)

    # 2. Check the symbol does not have any partial moves (prevents partially uninitialized use).
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

    # 6. Mark the symbol as consumed or partially moved, allowing for copies.
    if mark_symbols and not scope_handler.current_scope.get_symbol(symbol.type).is_copyable:
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
            generic_arguments.replace(generic_argument, new_argument, 1)

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
            final_arguments = TupleLiteralAst(argument.pos, TokenAst.dummy(TokenType.TkParenL), arguments[j:].map(lambda a: a.value).list(), TokenAst.dummy(TokenType.TkParenR))
            parameter_identifier = parameter_identifiers[0]  # todo: make .pop(0)?
            new_argument = FunctionArgumentNamedAst(argument.pos, parameter_identifier, TokenAst.dummy(TokenType.TkAssign), argument.convention, final_arguments)
            arguments = arguments[:j]
            arguments.append(new_argument)
            break

        # For a normal parameter, assign the next parameter identifier to the argument.
        else:
            parameter_identifier = parameter_identifiers.pop(0)
            new_argument = FunctionArgumentNamedAst(argument.pos, parameter_identifier, TokenAst.dummy(TokenType.TkAssign), argument.convention, argument.value)
            arguments.replace(argument, new_argument, 1)

    return arguments


def get_all_function_scopes(type_scope: Scope, identifier: "IdentifierAst", exclusive: bool = False) -> Seq[Tuple[Scope, "SupPrototypeNormalAst", List["GenericArgumentAst"]]]:
    from SPPCompiler.LexicalAnalysis.Lexer import Lexer
    from SPPCompiler.SyntacticAnalysis.Parser import Parser
    from SPPCompiler.SemanticAnalysis.ASTs import TypeAst, IdentifierAst, GenericArgumentNamedAst, TokenAst

    converted_identifier = Parser(Lexer(f"MOCK_{identifier}").lex(), "").parse_type_single().parse_once()

    sup_scopes = []
    generics = []

    match type_scope.name:

        # Functions in a namespace / global namespace; there will be no inheritable generics.
        case IdentifierAst():
            if Seq(type_scope.children).map(lambda s: s.name).contains(converted_identifier):
                sup_scope, ast = Seq(type_scope.children).filter(lambda s: s.name == converted_identifier).first().sup_scopes[0]
                sup_scopes.append((sup_scope, ast, []))

        # Functions that belong to a class (methods). Generics must come from the specific superimposition that these
        # methods belong to.
        case _:
            for sup_scope, _ in (type_scope.sup_scopes if not exclusive else type_scope._sup_scopes):
                if Seq(sup_scope.children).filter(lambda s: isinstance(s.name, TypeAst)).map(lambda s: s.name).contains(converted_identifier):
                    generics = Seq(sup_scope._symbol_table.all()).filter_to_type(TypeSymbol).filter(lambda t: t.is_generic and t.associated_scope)
                    generics = generics.map(GenericArgumentNamedAst.from_symbol)
                    fun_scope, ast = Seq(sup_scope.children).filter(lambda s: isinstance(s.name, TypeAst)).filter(lambda s: s.name == converted_identifier).first().sup_scopes[0]
                    sup_scopes.append((fun_scope, ast, generics))

            # Methods that have been overridden must be removed (ie use most derived method).
            ...

    return Seq(sup_scopes)


# def matching_function_types(this_member, that_member, current_scope: Scope, super_class_scope: Scope) -> bool:
#     this_member_has_self_parameter = this_member.body.members[0].parameters.get_self() is not None
#     that_member_has_self_parameter = that_member.body.members[0].parameters.get_self() is not None
#     t1 = this_member.super_class.types[-1].generic_arguments.arguments[-1].type.types[-1].generic_arguments.arguments[this_member_has_self_parameter:]
#     t2 = that_member.super_class.types[-1].generic_arguments.arguments[-1].type.types[-1].generic_arguments.arguments[that_member_has_self_parameter:]
#     return all(tt1.type.symbolic_eq(tt2.type, current_scope, super_class_scope) for tt1, tt2 in zip(t1, t2))


def get_owner_type_of_sup_block(identifier: "IdentifierAst", scope_handler: ScopeHandler) -> Optional[TypeSymbol]:
    self_symbol = None
    if identifier.types[-1].value.startswith("MOCK_"):
        if isinstance(scope_handler.current_scope.parent.name, (SupNormalIdentifier, SupInheritanceIdentifier)):
            class_type = scope_handler.current_scope.parent.name.this_class
            class_type.do_semantic_analysis(scope_handler)
            self_symbol = scope_handler.current_scope.get_symbol(class_type)

    return self_symbol


def substitute_generics_in_sup_scopes(sup_scopes: List[Tuple[Scope, "SupPrototypeAst"]], generic_arguments: List["GenericArgmentAst"], scope_handler: ScopeHandler, temp) -> List[Tuple[Scope, "SupPrototypeAst"]]:
    from SPPCompiler.SemanticAnalysis.ASTs import TypeAst, GenericArgumentNamedAst

    new_scopes = []
    for scope, sup_ast in sup_scopes:

        if isinstance(scope.name, TypeAst):

            # Rename the scope with generic substitutions.
            type_part_ast = copy.deepcopy(scope.name)
            for g in generic_arguments:
                type_part_ast.substitute_generics(g.identifier, g.type)

            # The substituted superclass may not exist yet with these generics.
            type_to_analyse = copy.deepcopy(sup_ast.super_class)
            for g in generic_arguments:
                type_to_analyse.substitute_generics(g.identifier, g.type)

            # type_part_ast != scope.name and type_part_ast.do_semantic_analysis(scope_handler)
            new_scope_name = type_part_ast

        elif isinstance(scope.name, SupNormalIdentifier):
            # Rename the scope with generic substitutions.
            type_part_ast = copy.deepcopy(scope.name.this_class)
            for g in generic_arguments:
                type_part_ast.substitute_generics(g.identifier, g.type)

            # type_part_ast != scope.name.this_class and type_part_ast.do_semantic_analysis(scope_handler)
            new_scope_name = SupNormalIdentifier(this_class=type_part_ast)

        elif isinstance(scope.name, SupInheritanceIdentifier):
            # Rename the scopes with generic substitutions.
            type_part_ast = copy.deepcopy(scope.name.this_class)
            super_class_part = copy.deepcopy(scope.name.super_class)
            for g in generic_arguments:
                type_part_ast.substitute_generics(g.identifier, g.type)
                super_class_part.substitute_generics(g.identifier, g.type)

            # type_part_ast != scope.name.this_class and type_part_ast.do_semantic_analysis(scope_handler)
            # super_class_part != scope.name.super_class and super_class_part.do_semantic_analysis(scope_handler)
            new_scope_name = SupInheritanceIdentifier(this_class=type_part_ast, super_class=super_class_part)

        else:
            raise NotImplementedError(f"Unsupported scope name type: {scope.name}")

        # Remove the generic arguments from the "sup" AST.
        new_sup_ast = sup_ast
        for p in new_sup_ast.generic_parameters.parameters.copy():
            for g in generic_arguments:
                if p.identifier == g.identifier:
                    new_sup_ast.generic_parameters.parameters.remove(p)

        # Create the new scope with the new name and parent scope.
        new_scope = Scope(name=new_scope_name, parent_scope=scope.parent)
        new_scope._children_scopes = scope._children_scopes
        new_scope._sup_scopes = substitute_generics_in_sup_scopes(scope._sup_scopes, generic_arguments, scope_handler, temp)
        new_scope._symbol_table = copy.copy(scope._symbol_table)
        new_scopes.append((new_scope, new_sup_ast))
        scope.parent.children.append(new_scope)

        for generic_argument in Seq(type_part_ast.types[-1].generic_arguments.arguments).filter_to_type(GenericArgumentNamedAst):
            generic_argument_type_symbol = scope_handler.current_scope.get_symbol(generic_argument.type)
            generic_parameter_type_symbol = TypeSymbol(
                name=generic_argument.identifier.types[-1],
                type=generic_argument_type_symbol.type,
                associated_scope=generic_argument_type_symbol.associated_scope,
                is_generic=True)
            new_scope.add_symbol(generic_parameter_type_symbol)

    return new_scopes


class FunctionConflictCheckType(Enum):
    InvalidOverload = 0
    InvalidOverride = 1


def check_for_conflicting_methods(
        type_scope: Scope,
        scope_handler: ScopeHandler,
        new_function: FunctionPrototypeAst,
        conflict_type: FunctionConflictCheckType) -> Optional[FunctionPrototypeAst]:

    """
    Check for conflicting methods in the current scope. This is used to detect a conflicting overload, or to ensure a
    valid override. The same logic is used, with minor tweaks.

    Args:
        type_scope: The scope to get all the existing functions from.
        scope_handler: The scope handler to use.
        new_function: The new function prototype that is being added.
        conflict_type: The conflict check to perform.

    Returns:
        The existing function prototype ast that is conflicting with the new function, if there is one, otherwise None.
    """

    # Get all the existing functions with the same identifier belonging to the type scope.
    exclusive = conflict_type == FunctionConflictCheckType.InvalidOverload
    existing_functions = get_all_function_scopes(type_scope, new_function._orig, exclusive).map(operator.itemgetter(1)).map(lambda sup: sup.body.members[0])
    existing_scopes    = get_all_function_scopes(type_scope, new_function._orig, exclusive).map(operator.itemgetter(0))

    # For overloads, the required parameters must have different types or conventions.
    if conflict_type == FunctionConflictCheckType.InvalidOverload:
        parameter_filter = lambda f: f.parameters.get_req()
        parameter_comparison = lambda p1, p2, s1: p1.type_declaration.symbolic_eq(p2.type_declaration, s1, scope_handler.current_scope) and p1.convention == p2.convention
        extra_check = lambda f1, f2: True

    # For overrides, all parameters must be direct matches.
    else:
        parameter_filter = lambda f: Seq(f.parameters.parameters)
        parameter_comparison = lambda p1, p2, s1: p1.type_declaration.symbolic_eq(p2.type_declaration, s1, scope_handler.current_scope) and p1.convention == p2.convention and p1.identifier == p2.identifier and type(p1) is type(p2)
        extra_check = lambda f1, f2: f1.return_type.symbolic_eq(f2.return_type, type_scope, scope_handler.current_scope)

    # Check each parameter set for each overload. 1 match means there is a conflict.
    for existing_scope, existing_function in existing_scopes.zip(existing_functions):
        parameter_set_1 = parameter_filter(existing_function)
        parameter_set_2 = parameter_filter(new_function)

        # Pre-checks that bypass type checking (parameter length, extra checks, same ast).
        if parameter_set_1.length != parameter_set_2.length: continue
        if not extra_check(existing_function, new_function): continue
        if existing_function is new_function: continue

        # Type-check the parameters (or already a match for 0-parameter functions).
        if parameter_set_1.length == 0 and parameter_set_2.length == 0:
            return existing_function
        if parameter_set_1.zip(parameter_set_2).all(lambda params: parameter_comparison(*params, existing_scope)):
            return existing_function

    # No conflicts found.
    return False


@dataclass(kw_only=True)
class SupInheritanceIdentifier:
    super_class: "TypeAst"
    this_class: "TypeAst"

    def __str__(self):
        return f"sup {self.super_class} on {self.this_class}"

    def __json__(self) -> str:
        return f"sup {self.super_class} on {self.this_class}"


@dataclass(kw_only=True)
class SupNormalIdentifier:
    this_class: "TypeAst"

    def __str__(self):
        return f"sup {self.this_class}"

    def __json__(self) -> str:
        return f"sup {self.this_class}"


class TypeInfer:
    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        pass


@dataclass(kw_only=True)
class InferredType:
    convention: Type["ConventionAst"]
    type: "TypeAst"

    def symbolic_eq(self, other: InferredType, scope: Scope, that_scope: Optional[Scope] = None) -> bool:
        return self.convention == other.convention and self.type.symbolic_eq(other.type, scope, that_scope)

    @staticmethod
    def from_type_ast(type_ast: "TypeAst") -> "InferredType":
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst
        return InferredType(convention=ConventionMovAst, type=type_ast)

    def __str__(self):
        return f"{self.convention.dummy()}{self.type}"

    def __hash__(self):
        return hash((self.convention, self.type))


__all__ = [
    "TypeInfer",
    "InferredType",
    "SupNormalIdentifier",
    "SupInheritanceIdentifier",
    "infer_generics_types",
    "ensure_memory_integrity",
    "convert_generic_arguments_to_named",
    "convert_function_arguments_to_named",
    "get_all_function_scopes",
    "check_for_conflicting_attributes",
    "matching_function_types",
    "get_owner_type_of_sup_block",
    "substitute_generics_in_sup_scopes",
]

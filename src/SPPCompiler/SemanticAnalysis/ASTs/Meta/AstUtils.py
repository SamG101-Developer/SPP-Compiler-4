from __future__ import annotations

import copy, operator
from collections import defaultdict
from dataclasses import dataclass, field
from fastenum import Enum
from typing import Dict, List, Optional, Tuple, Type

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.Utils.Sequence import Seq


def infer_generics_types(
        ast: "TypeAst",
        generic_parameter_identifiers: List["TypeAst"],
        explicit_generic_arguments: Dict["TypeAst", "TypeAst"],
        infer_source: Dict["IdentifierAst", "TypeAst"],
        infer_target: Dict["IdentifierAst", "TypeAst"],
        scope_handler: ScopeHandler,
        variadics: Optional[List["TypeAst"]] = None) -> Seq["GenericArgumentAst"]:

    """
    The infer_generic_types_2 is an upgraded generic inference method. It takes an "inference_from" dictionary, which
    might be a list of attributes and their values, and an "inference_to" dictionary, which would then be a list of
    attributes and their generic types.

    Explicit generic arguments are also given, so they can be analysed with the inferred generics, especially regarding
    conflict checking. For example:

    cls Point[T, U, V] {
        x: T
        y: U
        z: V
    }

    let p = Point(x=1, y="hello", z=False)

    the arguments:
        generic_parameter_identifiers: [T, U, V]
        explicit_generic_arguments: {}
        infer_source: {x: BigInt, y: Str, z: Bool}
        infer_target: {x: T, y: U, z: V}

    Args:
        ast:
        generic_parameter_identifiers: The list of generic parameters to infer types for.
        explicit_generic_arguments: Already defined generic arguments.
        infer_source: Source values to infer generic types from.
        infer_target: Target generic types to infer to.
        scope_handler: The scope handler.
        variadics: Any infer_source sources that are variadic (only use a single type-element).

    Returns:
        The complete, analysed list of generic arguments.
    """

    from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
    from SPPCompiler.SemanticAnalysis.ASTs import GenericArgumentNamedAst, TokenAst, TypeAst

    if isinstance(ast, TypeAst) and ast.without_generics() == CommonTypes.tuple([]):
        all_generic_arguments = [GenericArgumentNamedAst(-1, identifier.types[-1].to_identifier(), TokenAst.dummy(TokenType.TkAssign), type) for identifier, type in explicit_generic_arguments.items()]
        return Seq(all_generic_arguments)

    # Infer all the generics from the source to the target.
    inferred_generics_arguments = defaultdict(Seq)
    variadics = variadics or []

    for generic_parameter_identifier in generic_parameter_identifiers:
        for infer_target_identifier, infer_target_type in infer_target.items():
            if infer_target_identifier in infer_source.keys() and infer_target_type == generic_parameter_identifier:
                inferred_generics_arguments[generic_parameter_identifier].append(infer_source[infer_target_identifier])
            elif infer_target_identifier in infer_source.keys() and infer_target_type.contains_generic(generic_parameter_identifier):
                inferred_generics_arguments[generic_parameter_identifier].append(infer_source[infer_target_identifier].get_generic_argument(infer_target[infer_target_identifier].get_generic_parameter_for_generic_argument(generic_parameter_identifier)))

            # Reduce the variadic to a single type-element. Todo: Variadic-type variadic values shouldn't be reduced?
            if infer_target_identifier in variadics:
                inferred_generics_arguments[generic_parameter_identifier][-1] = inferred_generics_arguments[generic_parameter_identifier][-1].types[-1].generic_arguments.arguments[0].type

    # Remove all "None" items from each list of inferred generics.
    for _, inferred_generics_argument_types in inferred_generics_arguments.items():
        inferred_generics_argument_types.remove_none()
    for inferred_generics_argument_identifier, inferred_generics_argument_types in inferred_generics_arguments.copy().items():
        if inferred_generics_argument_types.empty():
            del inferred_generics_arguments[inferred_generics_argument_identifier]

    # Check there are no conflicting inferred types on the same generic.
    for inferred_generic_name, inferred_generic_types in inferred_generics_arguments.items():
        for check_conflict_inferred_generic_type in inferred_generics_arguments[inferred_generic_name]:
            if not inferred_generic_types[0].symbolic_eq(check_conflict_inferred_generic_type, scope_handler.current_scope):
                raise SemanticErrors.CONFLICTING_GENERIC_INFERENCE(inferred_generic_name, inferred_generic_types[0], check_conflict_inferred_generic_type)

    # Convert the inferred generic arguments from {T -> (...U)} to {T -> U}.
    inferred_generics_arguments = {k: v[0] for k, v in inferred_generics_arguments.items()}

    # Check there are no conflicting inferred types with explicit types.
    for inferred_generic_name, inferred_generic_type in inferred_generics_arguments.items():
        if inferred_generic_name in explicit_generic_arguments:
            if not inferred_generic_type.symbolic_eq(explicit_generic_arguments[inferred_generic_name], scope_handler.current_scope):
                raise SemanticErrors.CONFLICTING_GENERIC_INFERENCE(inferred_generic_name, inferred_generic_type, explicit_generic_arguments[inferred_generic_name])

    # Check all generic parameters have been inferred or explicitly defined.
    for generic_parameter_identifier in generic_parameter_identifiers:
        if generic_parameter_identifier not in inferred_generics_arguments | explicit_generic_arguments:
            raise SemanticErrors.MISSING_GENERIC_ARGUMENT(ast, generic_parameter_identifier)

    # Merge and format the generic arguments.
    all_generic_arguments = inferred_generics_arguments | explicit_generic_arguments
    all_generic_arguments = [GenericArgumentNamedAst(-1, identifier.types[-1].to_identifier(), TokenAst.dummy(TokenType.TkAssign), type) for identifier, type in all_generic_arguments.items()]
    return Seq(all_generic_arguments)


def ensure_memory_integrity(
        entire_ast: Ast, value_ast: Ast, move_ast: Ast, scope_handler: ScopeHandler,
        check_move: bool = True, check_partial_move: bool = True, check_move_from_borrowed_context: bool = True,
        check_pinned_move: bool = True, mark_symbols: bool = True) -> None:

    from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, PostfixExpressionAst, TypeAst, TupleLiteralAst, ArrayLiteralNElementAst
    from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
    from SPPCompiler.SemanticAnalysis.Utils.Symbols import NamespaceSymbol

    # Get the symbol, for Identifiers and PostfixExpressions. If the value isn't either of these, then the memory rules
    # don't apply, so all the checks are skipped.
    symbol = scope_handler.current_scope.get_outermost_variable_symbol(value_ast)
    if isinstance(value_ast, TypeAst) or isinstance(symbol, NamespaceSymbol):
        raise SemanticErrors.INVALID_TYPE_EXPRESSION(value_ast)
    if isinstance(value_ast, (TupleLiteralAst, ArrayLiteralNElementAst)):
        for item in value_ast.items:
            ensure_memory_integrity(entire_ast, item, move_ast, scope_handler, check_move, check_partial_move, check_move_from_borrowed_context, check_pinned_move, mark_symbols)
        return
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
        FunctionArgumentNamedAst, FunctionArgumentNormalAst, TokenAst, TupleLiteralAst, FunctionParameterVariadicAst, FunctionParameterOptionalAst)

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

    # Add default parameters, and an empty tuple for the variadic parameter.
    for parameter in Seq(parameters).filter_to_type(FunctionParameterOptionalAst):
        if parameter.identifier_for_param() not in arguments.map(lambda a: a.identifier):
            default_name = parameter.identifier_for_param()
            default_value = parameter.default_value
            new_argument = FunctionArgumentNamedAst(parameter.pos, default_name, TokenAst.dummy(TokenType.TkAssign), parameter.convention, default_value)
            arguments.append(new_argument)
    if parameters and isinstance(parameters[-1], FunctionParameterVariadicAst) and not arguments.map(lambda a: a.identifier).contains(parameters[-1].identifier_for_param()):
        final_arguments = TupleLiteralAst(parameters[-1].pos, TokenAst.dummy(TokenType.TkParenL), [], TokenAst.dummy(TokenType.TkParenR))
        new_argument = FunctionArgumentNamedAst(parameters[-1].pos, parameters[-1].identifier_for_param(), TokenAst.dummy(TokenType.TkAssign), parameters[-1].convention, final_arguments)
        arguments.append(new_argument)

    return arguments


def get_all_function_scopes(type_scope: Scope, identifier: "IdentifierAst", exclusive: bool = False) -> Seq[Tuple[Scope, "SupPrototypeNormalAst", List["GenericArgumentAst"]]]:
    from SPPCompiler.LexicalAnalysis.Lexer import Lexer
    from SPPCompiler.SyntacticAnalysis.Parser import Parser
    from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, GenericArgumentNamedAst, ClassPrototypeAst, SupPrototypeInheritanceAst
    from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol

    converted_identifier = Parser(Lexer(f"MOCK_{identifier}").lex(), "").parse_type_single().parse_once()
    sup_scopes = []
    generics = []

    match type_scope.name:

        # Functions in a namespace / global namespace; there will be no inheritable generics.
        case IdentifierAst():
            for scope in type_scope.ancestors:
                if Seq(scope.children).map(lambda s: s.name).contains(converted_identifier):
                    sup_scope, ast = Seq(scope.children).filter(lambda s: s.name == converted_identifier).first()._sup_scopes[0]
                    sup_scopes.append((sup_scope, ast, []))

        # Functions that belong to a class (methods). Generics must come from the specific superimposition that these
        # methods belong to.
        case _:
            all_sup_scopes = type_scope.sup_scopes if not exclusive else type_scope._sup_scopes
            for sup_scope, sup_ast in all_sup_scopes:
                if isinstance(sup_ast, ClassPrototypeAst): continue

                if valid := Seq(sup_ast.body.members).filter_to_type(SupPrototypeInheritanceAst).filter(lambda m: m.identifier == converted_identifier):
                    generics = Seq(sup_scope._symbol_table.all()).filter_to_type(TypeSymbol).filter(lambda t: t.is_generic and t.associated_scope)
                    generics = generics.map(GenericArgumentNamedAst.from_symbol)
                    fun_scope, ast = valid.first()._scope, valid.first()
                    sup_scopes.append((fun_scope, ast, generics))

            # Methods that have been overridden must be removed (ie use most derived method).
            # Todo: maybe return a breadth-first search of the scopes to ensure the most derived method is used.
            ...

    return Seq(sup_scopes)


def get_owner_type_of_sup_block(identifier: "TypeAst", scope_handler: ScopeHandler) -> Optional[TypeSymbol]:
    self_symbol = None
    if identifier.types[-1].value.startswith("MOCK_"):
        if isinstance(scope_handler.current_scope.parent.name, (SupNormalIdentifier, SupInheritanceIdentifier)):
            class_type = scope_handler.current_scope.parent.name.this_class
            class_type.do_semantic_analysis(scope_handler)
            self_symbol = scope_handler.current_scope.get_symbol(class_type)

    return self_symbol


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
    existing = get_all_function_scopes(type_scope, new_function._orig, conflict_type == FunctionConflictCheckType.InvalidOverload)
    existing_scopes    = existing.map(operator.itemgetter(0))
    existing_functions = existing.map(operator.itemgetter(1)).map(lambda sup: sup.body.members[0])
    existing_generics  = existing.map(operator.itemgetter(2))

    # For overloads, the required parameters must have different types or conventions.
    if conflict_type == FunctionConflictCheckType.InvalidOverload:
        parameter_filter = lambda f: f.parameters.get_req()
        parameter_comparison = lambda p1, p2, s1: p1.type_declaration.symbolic_eq(p2.type_declaration, s1, scope_handler.current_scope) and p1.convention == p2.convention
        extra_check = lambda f1, f2: True

    # For overrides, all parameters must be direct matches.
    else:
        parameter_filter = lambda f: f.parameters.get_non_self()
        parameter_comparison = lambda p1, p2, s1: p1.type_declaration.symbolic_eq(p2.type_declaration, s1, scope_handler.current_scope) and p1.convention == p2.convention and p1.identifier_for_param() == p2.identifier_for_param() and type(p1) is type(p2)
        extra_check = lambda f1, f2: f1.return_type.symbolic_eq(f2.return_type, type_scope, scope_handler.current_scope)  # Todo: Check self conventions match here too if they both have one.

    # Check each parameter set for each overload. 1 match means there is a conflict.
    for (existing_scope, existing_function), existing_generic in existing_scopes.zip(existing_functions).zip(existing_generics):
        parameter_set_1 = parameter_filter(existing_function)
        parameter_set_2 = parameter_filter(new_function)

        parameter_set_1 = copy.deepcopy(parameter_set_1)
        for p in parameter_set_1:
            for g in existing_generic:
                p.type_declaration = p.type_declaration.substituted_generics(g.identifier, g.type)

        # Todo: Check both are "fun" or "cor".

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
    return None


@dataclass(kw_only=True)
class SupNormalIdentifier:
    this_class: "TypeAst"

    def __str__(self):
        return f"sup {self.this_class}"

    def __json__(self) -> str:
        return f"sup {self.this_class}"


@dataclass(kw_only=True)
class SupInheritanceIdentifier(SupNormalIdentifier):
    super_class: "TypeAst"
    this_class: "TypeAst"

    def __str__(self):
        return f"sup {self.this_class} ext {self.super_class}"

    def __json__(self) -> str:
        return f"sup {self.this_class} ext {self.super_class}"


class Visibility(Enum):
    Public = 0
    Protected = 1
    Packaged = 2
    Private = 3

    def __json__(self) -> str:
        return self.name.lower()


@dataclass(kw_only=True)
class VisibilityEnabled:
    _visibility: Visibility = field(default=Visibility.Private, init=False, repr=False)


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


def substitute_sup_scopes(sup_scopes: List[Tuple[Scope, SupPrototypeAst]], generics: List[GenericArgumentAst], scope_handler: ScopeHandler) -> List[Tuple[Scope, SupPrototypeAst]]:
    new_sup_scopes = []
    for sup_scope, sup_ast in sup_scopes:
        type_symbol = sup_scope.get_symbol(sup_ast.identifier.without_generics())
        new_scope = create_generic_scope(None, type_symbol, sup_scope, generics, scope_handler)
        new_sup_scopes.append((new_scope, sup_ast))
    return new_sup_scopes


def create_generic_scope(type: TypeAst, type_symbol: TypeSymbol, type_scope: Scope, generics: List[GenericArgumentAst], scope_handler: ScopeHandler) -> Scope:
    # Todo: Simplify in adding vs replacing generics in TypeAst-identifier scopes.
    from SPPCompiler.SemanticAnalysis.ASTs import TypeAst, GenericArgumentNamedAst
    from SPPCompiler.SemanticAnalysis.Utils.Scopes import Scope
    from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol, TypeAliasSymbol

    # Duplicate the scope, using a copy of the existing name, the same parent, and link the non-generic scope.
    new_scope = Scope(copy.deepcopy(type_scope.name), parent_scope=type_scope.parent, non_generic_scope=type_scope, handler=scope_handler)

    # Handling type-scope substitutions (class prototypes).
    old_generics = Seq(generics)
    if isinstance(new_scope.name, TypeAst):

        # Save the list of generics provided. The replacement generics are the type-symbols existing in scope.
        if (type and type.without_generics() != CommonTypes.tuple([])) or not type:
            generics = Seq(generics).filter(lambda g: type_scope.has_symbol(g.identifier) and type_scope.get_symbol(g.identifier).is_generic).list()

        # For types without their generics, add them in.
        if not new_scope._scope_name.types[-1].generic_arguments.arguments:
            new_scope._scope_name.types[-1].generic_arguments.arguments = generics.copy()

        # Otherwise, substitute them in.
        else:
            if (type and type.without_generics() != CommonTypes.tuple([])) or not type:
                for generic in old_generics:
                    new_scope._scope_name = new_scope.name.substituted_generics(generic.identifier, generic.type)

    # For superimposition scopes, substitute the generics in the superimposition identifier and potential superclass.
    elif isinstance(new_scope.name, SupNormalIdentifier):
        for generic in generics:
            new_scope._scope_name.this_class = new_scope.name.this_class.substituted_generics(generic.identifier, generic.type)
            if isinstance(new_scope.name, SupInheritanceIdentifier):
                new_scope._scope_name.super_class = new_scope.name.super_class.substituted_generics(generic.identifier, generic.type)

    # Either use an existing scope if the new one exists, or add the new scope to the parent scope.
    if scope_handler.current_scope.has_symbol(new_scope.name):
        return scope_handler.current_scope.get_symbol(new_scope.name).associated_scope
    if new_scope.name != type_scope.name:
        type_scope.parent.children.append(new_scope)
    else:
        return type_scope

    # Recursively substitute the super scopes.
    if old_generics and type_scope._sup_scopes:
        new_scope._sup_scopes = substitute_sup_scopes(type_scope._sup_scopes.copy(), old_generics, scope_handler)

    # Handle the "Self" type, if it is a type-scope.
    if isinstance(new_scope.name, TypeAst) and not type_scope.parent.has_symbol(new_scope.name.types[-1]):
        new_symbol_type = copy.deepcopy(type_symbol.type)

        # Add the new "Self" type, and add the new scope to the parent scope.
        new_scope.add_symbol(TypeSymbol(name=CommonTypes.self().types[-1], type=new_symbol_type, associated_scope=new_scope, is_copyable=type_symbol.is_copyable))
        match type_symbol:
            case TypeAliasSymbol(): type_scope.parent.add_symbol(TypeAliasSymbol(name=new_scope.name.types[-1], type=new_symbol_type, associated_scope=new_scope, is_copyable=type_symbol.is_copyable, old_type=type_symbol.old_type, old_associated_scope=type_symbol.old_associated_scope))
            case TypeSymbol()     : type_scope.parent.add_symbol(TypeSymbol(name=new_scope.name.types[-1], type=new_symbol_type, associated_scope=new_scope, is_copyable=type_symbol.is_copyable))
        type_scope = new_scope

    # Register the new generic arguments against the generic parameters in the new scope.
    if (type and type.without_generics() != CommonTypes.tuple([])) or not type:
        if not isinstance(new_scope.name, TypeAst):
            generics += Seq(new_scope.name.this_class.types[-1].generic_arguments.arguments).filter_to_type(GenericArgumentNamedAst).list()

        for generic_argument in generics:
            generic_argument_type_symbol = scope_handler.current_scope.get_symbol(generic_argument.type)
            generic_parameter_type_symbol = TypeSymbol(name=generic_argument.identifier.types[-1], type=generic_argument_type_symbol.type, associated_scope=generic_argument_type_symbol.associated_scope, is_generic=True)
            new_scope.add_symbol(generic_parameter_type_symbol)

    return new_scope


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
    "get_owner_type_of_sup_block",
]

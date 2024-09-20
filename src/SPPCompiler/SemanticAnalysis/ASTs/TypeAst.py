from __future__ import annotations

import copy
import hashlib
from dataclasses import dataclass
from typing import Iterable, List, Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType, convert_generic_arguments_to_named, \
    infer_generics_types, create_generic_scope
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import Scope, ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol, VariableSymbol, NamespaceSymbol, TypeAliasSymbol
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class TypeAst(Ast, SemanticAnalyser, TypeInfer):
    """
    A TypeSingleIdentifier is a single type, ie `std.Vec[T]`. It is made up of a sequence of TypePartAst's, which can be
    either IdentifierAsts (namespace parts), GenericIdentifierAsts (type parts), or TokenAsts (numbers for tuple
    access).

    Attributes:
        namespace: The namespace of the type, i.e. `std` in `std::Vec[T]`.
        types: The type parts of the type, i.e. `Vec`, `Iterator`, `T` in `std::Vec[T]::Iterator`.
    """

    namespace: List["IdentifierAst"]
    types: List["TypePartAst"]

    def __post_init__(self):
        self.namespace = self.namespace or []

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypeSingleAst.
        s = ""
        s += f"{Seq(self.namespace).print(printer, "::")}::" if self.namespace else ""
        s += f"{Seq(self.types).print(printer, "::")}"
        return s

    def get_generic_argument(self, generic_parameter_identifier: TypeAst) -> TypeAst:
        from SPPCompiler.SemanticAnalysis.ASTs import GenericArgumentAst

        def custom_iterate(type_single: TypeAst) -> Iterable[GenericArgumentAst]:
            for part in type_single.types:
                for generic_argument in part.generic_arguments.arguments:
                    yield generic_argument
                    yield from custom_iterate(generic_argument.type)

        for generic in custom_iterate(self):
            if generic.identifier == generic_parameter_identifier:
                return generic.type

    def get_generic_parameter_for_generic_argument(self, generic_argument_type: TypeAst) -> TypeAst:
        from SPPCompiler.SemanticAnalysis.ASTs import GenericArgumentAst

        def custom_iterate(type_single: TypeAst) -> Iterable[GenericArgumentAst]:
            for part in type_single.types:
                for generic_argument in part.generic_arguments.arguments:
                    yield generic_argument
                    yield from custom_iterate(generic_argument.type)

        for generic in custom_iterate(self):
            if generic.type == generic_argument_type:
                return generic.identifier

    def do_semantic_analysis(self, scope_handler: ScopeHandler, generic_infer_from=None, generic_map_to=None, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import GenericIdentifierAst

        # Determine the scope to use for the type.
        match self.namespace:
            case []: type_scope = scope_handler.current_scope
            case _ : type_scope = scope_handler.get_namespaced_scope(self.namespace)

        # Check the type-scope exists (valid namespace).
        temp_namespace = []
        for part in self.namespace:
            temp_namespace.append(part)
            if not scope_handler.get_namespaced_scope(temp_namespace):
                valid_namespaces = Seq(scope_handler.get_namespaced_scope(temp_namespace[:-1]).all_symbols(exclusive=True))
                valid_namespaces = valid_namespaces.filter_to_type(NamespaceSymbol).map(lambda s: s.name).map(str).list()
                raise SemanticErrors.UNKNOWN_IDENTIFIER(part, valid_namespaces, "namespace")

        # Move through each type, ensuring it exists at least in non-generic form.
        for i, type_part in enumerate(self.types):
            if isinstance(type_part, GenericIdentifierAst):
                # If the type doesn't exist, raise an error.
                if not type_scope.has_symbol(type_part.without_generics()):
                    raise SemanticErrors.UNKNOWN_IDENTIFIER(type_part.without_generics(), [], "type")

                type_symbol = type_scope.get_symbol(type_part.without_generics(), ignore_alias=True)
                type_scope = type_symbol.associated_scope

                # Generic parameters won't have a scope, so skip them.
                if type_symbol.is_generic: continue

                # Name the generic arguments using the standard conversion.
                type_part.generic_arguments.arguments = convert_generic_arguments_to_named(
                    generic_arguments=Seq(type_part.generic_arguments.arguments),
                    generic_parameters=Seq(type_symbol.type.generic_parameters.parameters),
                    generic_parameters_scope=type_scope).list()

                # Infer any generics from object initialisation, and load default generics into the type.
                type_part.generic_arguments.arguments = infer_generics_types(
                    ast=type_part,
                    generic_parameter_identifiers=Seq(type_symbol.type.generic_parameters.get_req()).map(lambda p: p.identifier).list(),
                    explicit_generic_arguments=Seq(type_part.generic_arguments.arguments).map(lambda a: (a.identifier, a.type)).dict(),
                    infer_source=generic_infer_from or {}, infer_target=generic_map_to or {}, scope_handler=scope_handler).list()

                # Analyse all the generic arguments.
                if self.without_generics() != CommonTypes.tuple([]):
                    type_part.generic_arguments.do_semantic_analysis(scope_handler, **kwargs)
                elif type_part.generic_arguments.arguments:
                    type_part.generic_arguments = type_part.generic_arguments.arguments[-1].type.types[-1].generic_arguments
                    type_part.generic_arguments.do_semantic_analysis(scope_handler, **kwargs)

                # If the generic type doesn't exist, create a symbol and scope for it.
                if not type_scope.parent.has_symbol(type_part):
                    new_scope = create_generic_scope(self, type_symbol, type_scope, type_part.generic_arguments.arguments, scope_handler)

                    # Handle type-alias's old types (regarding generic substitution).
                    if isinstance(new_scope.associated_type_symbol, TypeAliasSymbol):
                        for generic_argument in type_part.generic_arguments.arguments:
                            new_scope.associated_type_symbol.old_type = new_scope.associated_type_symbol.old_type.substituted_generics(generic_argument.identifier, generic_argument.type)

                    if isinstance(new_scope.associated_type_symbol, TypeAliasSymbol):
                        new_scope.associated_type_symbol.old_type.do_semantic_analysis(scope_handler, **kwargs)

            else:
                # Ensure that the type is a tuple for numerical-indexing.
                dummy_type = TypeAst(self.pos, self.namespace, self.types[:i])
                if dummy_type.without_generics() != CommonTypes.tuple([]):
                    raise SemanticErrors.NUMERICAL_MEMBER_ACCESS_TYPE(dummy_type, type_part, dummy_type)

                index = int(type_part.token.token_metadata)
                if index >= len(dummy_type.types[-1].generic_arguments.arguments):
                    raise SemanticErrors.NUMERICAL_MEMBER_ACCESS_OUT_OF_BOUNDS(dummy_type, type_part, dummy_type)

                new_type = dummy_type.types[-1].generic_arguments.arguments[index].type
                new_scope = type_scope.get_symbol(new_type).associated_scope
                type_scope = new_scope

        # Finally, add the namespace to this symbol (fully qualifying it).
        # from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst

        # if type_scope and isinstance(type_scope.parent.name, IdentifierAst):
        #     self.namespace = type_scope.scopes_as_namespace

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst
        return InferredType(convention=ConventionMovAst, type=self)

    def without_generics(self) -> TypeAst:
        from SPPCompiler.SemanticAnalysis.ASTs import GenericIdentifierAst

        final_part = self.types[-1]
        match final_part:
            case GenericIdentifierAst(): final_part = final_part.without_generics()
            case _: pass

        return TypeAst(self.pos, self.namespace, self.types[:-1] + [final_part])

    def contains_generic(self, generic: TypeAst) -> bool:
        for part in self:
            if part == generic.types[-1]:
                return True

    def _substitute_generics(self, generic_parameter_identifier: TypeAst, generic_argument_type: TypeAst) -> TypeAst:
        from SPPCompiler.SemanticAnalysis.ASTs import GenericIdentifierAst

        # Get all the substitutable parts of this type (not namespace parts or numeric indexing).
        type_parts = Seq(self.types).filter_to_type(GenericIdentifierAst)

        # Check if this type directly matches the generic parameter identifier (by name not symbol).
        if self.without_generics() == generic_parameter_identifier.without_generics():
            self.namespace = generic_argument_type.namespace
            self.types = generic_argument_type.types
            return self

        # Otherwise, iterate through the generic arguments and try to substitute each one.
        for part in type_parts:
            for g in part.generic_arguments.arguments if part.generic_arguments else []:
                g.type._substitute_generics(generic_parameter_identifier, generic_argument_type)

        # Return this type, internally modified.
        return self

    def substituted_generics(self, generic_parameter_identifier: TypeAst, generic_argument_type: TypeAst) -> TypeAst:
        # Create a new TypeAst with the substituted generic.
        new_type = copy.deepcopy(self)
        new_type._substitute_generics(generic_parameter_identifier, generic_argument_type)
        return new_type

    def symbolic_eq(self, that: TypeAst, this_scope: Scope, that_scope: Optional[Scope] = None) -> bool:
        # Default the "that_scope" to the "this_scope" if not provided.
        that_scope = that_scope or this_scope
        this_symbol = this_scope.get_symbol(self)
        that_symbol = that_scope.get_symbol(that)

        # print("-" * 100)
        # print(self, that, this_scope, that_scope)
        # print(this_symbol, that_symbol)

        # Special cases for union types.
        if this_symbol.fq_type.without_generics() == CommonTypes.var([]) and this_symbol.name.generic_arguments.arguments:
            for generic_argument in this_symbol.name.generic_arguments.arguments[-1].type.types[-1].generic_arguments.arguments:
                if generic_argument.type.symbolic_eq(that, this_scope, that_scope):
                    return True

        # Compare each type by getting the AST from the correct scope.
        this_type = this_symbol.type
        that_type = that_symbol.type
        return this_type is that_type

    def __iter__(self):
        # Iterate the parts, and recursively the parts of generic parameters
        def iterate(type_single: TypeAst):
            for part in type_single.types:
                yield part
                for g in part.generic_arguments.arguments:
                    yield from iterate(g.type)

        return iterate(self)

    def __eq__(self, that: TypeAst) -> bool:
        # Check both ASTs are the same type and have the same parts.
        return isinstance(that, TypeAst) and self.namespace == that.namespace and self.types == that.types

    def __hash__(self):
        # Hash the namespace and types of the type.
        return int.from_bytes(hashlib.md5("".join([str(p) for p in self.namespace + self.types]).encode()).digest())

    def __json__(self) -> str:
        return f"cls {self.print(AstPrinter())}"

    def __lt__(self, other):
        return str(self) < str(other)


__all__ = ["TypeAst"]

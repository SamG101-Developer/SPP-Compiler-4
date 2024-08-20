from __future__ import annotations

import copy
import hashlib
from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType, convert_generic_arguments_to_named, infer_generics_types
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import Scope, ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol, VariableSymbol
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

    def substitute_generics(self, from_ty: TypeAst, to_ty: TypeAst) -> TypeAst:
        from SPPCompiler.SemanticAnalysis.ASTs import GenericIdentifierAst

        # Substitute the generic type "from_ty" with "to_ty" in the type identifier (recursively).
        type_parts = Seq(self.types).filter_to_type(GenericIdentifierAst)

        # Try to substitute the type, i.e. if "from_ty" has been reached. Cant have a generic type with generic
        # arguments so return self.
        if self.without_generics() == from_ty.without_generics():
            self.namespace = to_ty.namespace
            self.types = to_ty.types
            return self

        # Otherwise, iterate the generic arguments and try to substitute the type in each one.
        for part in type_parts:
            for g in part.generic_arguments.arguments if part.generic_arguments else []:
                g.type.substitute_generics(from_ty, to_ty)

        # Return the modified type (self).
        return self

    def do_semantic_analysis(self, scope_handler: ScopeHandler, generic_infer_from=None, generic_map_to=None, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import GenericIdentifierAst

        # Determine the scope to use for the type.
        match self.namespace:
            case []: type_scope = scope_handler.current_scope
            case _ : type_scope = scope_handler.get_namespaced_scope(self.namespace)

        # Move through each type, ensuring it exists at least in non-generic form.
        for i, type_part in enumerate(self.types):
            if isinstance(type_part, GenericIdentifierAst):

                # If the type doesn't exist, raise an error.
                if not type_scope.has_symbol(type_part.without_generics()):
                    raise SemanticErrors.UNKNOWN_IDENTIFIER(type_part, [], "type")

                type_symbol = type_scope.get_symbol(type_part.without_generics())
                type_scope = type_symbol.associated_scope

                # Generic parameters won't have a scope, so skip them.
                if type_symbol.is_generic:
                    continue

                # Name the generic arguments using the standard conversion.
                type_part.generic_arguments.arguments = convert_generic_arguments_to_named(
                    generic_arguments=Seq(type_part.generic_arguments.arguments),
                    generic_parameters=Seq(type_symbol.type.generic_parameters.parameters),
                    generic_parameters_scope=type_scope).list()

                # Infer any generics from object initialisation, and load default generics into the type.
                type_part.generic_arguments.arguments = infer_generics_types(
                    ast=type_part,
                    generic_parameters=Seq(type_symbol.type.generic_parameters.get_req()).map(lambda p: p.identifier).list(),
                    explicit_generic_arguments=Seq(type_part.generic_arguments.arguments).map(lambda a: (a.identifier, a.type)).dict(),
                    infer_from=generic_infer_from or {}, map_to=generic_map_to or {}, scope_handler=scope_handler).list()

                # Analyse all the generic arguments.
                if self.without_generics() != CommonTypes.tuple([]):
                    type_part.generic_arguments.do_semantic_analysis(scope_handler, **kwargs)
                elif type_part.generic_arguments.arguments:
                    type_part.generic_arguments = type_part.generic_arguments.arguments[-1].type.types[-1].generic_arguments
                    type_part.generic_arguments.do_semantic_analysis(scope_handler, **kwargs)

                # If the generic type doesn't exist, create a symbol and scope for it.
                if not type_scope.parent.has_symbol(type_part):

                    # Create the new scope for this type and add it to the parent scope.
                    new_scope = Scope(copy.deepcopy(type_scope.name), parent_scope=type_scope.parent)
                    new_scope._scope_name.types[-1].generic_arguments.arguments = type_part.generic_arguments.arguments
                    new_scope._sup_scopes = type_scope._sup_scopes
                    new_scope._normal_sup_scopes = type_scope._normal_sup_scopes
                    new_scope._children_scopes = type_scope._children_scopes
                    new_scope._symbol_table = copy.deepcopy(type_scope._symbol_table)

                    # Create the new symbol.
                    new_cls_ast = copy.deepcopy(type_symbol.type)

                    # Add the new "Self" type, and add the new scope to the parent scope.
                    new_scope.add_symbol(TypeSymbol(name=CommonTypes.self(), type=new_cls_ast, associated_scope=new_scope))
                    type_scope.parent.add_symbol(TypeSymbol(name=new_scope.name, type=new_cls_ast, associated_scope=new_scope))
                    type_scope.parent.children.append(new_scope)
                    type_scope = new_scope

                    # Register the new generic arguments against the generic parameters in the new scope.
                    if self.without_generics() != CommonTypes.tuple([]):
                        for generic_argument in type_part.generic_arguments.arguments:

                            generic_argument_type_symbol = scope_handler.current_scope.get_symbol(generic_argument.type)
                            generic_parameter_type_symbol = TypeSymbol(
                                name=generic_argument.identifier,
                                type=generic_argument_type_symbol.type,
                                associated_scope=generic_argument_type_symbol.associated_scope,
                                is_generic=True)
                            new_scope.add_symbol(generic_parameter_type_symbol)

                            # Substitute the attribute symbol's generic types.
                            for attribute_symbol in Seq(new_scope.all_symbols()).filter_to_type(VariableSymbol):
                                attribute_symbol.type.substitute_generics(generic_argument.identifier, generic_argument.type)

                            # Substitute the ast attribute's generic types.
                            for attribute_ast in new_cls_ast.body.members:
                                attribute_ast.type_declaration.substitute_generics(generic_argument.identifier, generic_argument.type)

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
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst
        if type_scope and isinstance(type_scope.parent.name, IdentifierAst):
            self.namespace = type_scope.scopes_as_namespace

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

    def symbolic_eq(self, that: TypeAst, this_scope: Scope, that_scope: Optional[Scope] = None) -> bool:
        # Special cases for union types.
        if self.without_generics() == CommonTypes.var([]) and self.types[-1].generic_arguments.arguments:
            for generic_argument in self.types[-1].generic_arguments.arguments[-1].type.types[-1].generic_arguments.arguments:
                if generic_argument.type.symbolic_eq(that, this_scope, that_scope):
                    return True

        # Default the "that_scope" to the "this_scope" if not provided.
        that_scope = that_scope or this_scope

        # Compare each type by getting the AST from the correct scope.
        this_type = this_scope.get_symbol(self).type
        that_type = that_scope.get_symbol(that).type
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
        printer = AstPrinter()
        return self.print(printer)


__all__ = ["TypeAst"]

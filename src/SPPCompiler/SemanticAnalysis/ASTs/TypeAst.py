from __future__ import annotations

import copy
import hashlib
from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import convert_generic_arguments_to_named, infer_generics_types
from SPPCompiler.SemanticAnalysis.Utils.Scopes import Scope, ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol, VariableSymbol
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class TypeAst(Ast, SemanticAnalyser):
    """
    A TypeSingleIdentifier is a single type, ie `std.Vec[T]`. It is made up of a sequence of TypePartAst's, which can be
    either IdentifierAsts (namespace parts), GenericIdentifierAsts (type parts), or TokenAsts (numbers for tuple
    access).

    Attributes:
        parts: The parts that make up the type identifier.
    """

    parts: List["TypePartAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypeSingleAst.
        s = ""
        s += f"{Seq(self.parts).print(printer, '.')}"
        return s

    def substitute_generics(self, from_ty: TypeAst, to_ty: TypeAst) -> TypeAst:
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst

        # Substitute the generic type "from_ty" with "to_ty" in the type identifier (recursively).
        namespace_parts = Seq(self.parts).filter(lambda p: isinstance(p, IdentifierAst)).value
        self._substitute_generics(from_ty, to_ty)
        return self

    def _substitute_generics(self, from_ty: TypeAst, to_ty: TypeAst) -> TypeAst:
        from SPPCompiler.SemanticAnalysis.ASTs import GenericIdentifierAst

        # Substitute the generic type "from_ty" with "to_ty" in the type identifier (recursively).
        type_parts = [(i, p) for i, p in enumerate(self.parts) if isinstance(p, GenericIdentifierAst)]
        replace_n = len(self.parts) - len(type_parts)

        # Try to substitute the type, i.e. if "from_ty" has been reached.
        if self.without_generics() == from_ty.without_generics():
            self.parts = to_ty.parts

        # Otherwise, iterate the generic arguments and try to substitute the type in each one.
        # TODO: put this "for" in an "else"?
        for i, part in type_parts:
            for g in part.generic_arguments.arguments if part.generic_arguments else []:
                g.type._substitute_generics(from_ty, to_ty)

        # Return the modified type (self).
        return self

    def do_semantic_analysis(self, scope_handler: ScopeHandler, verify_generics: bool = True, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst, GenericArgumentGroupAst

        # Check if this type exists both with and without the generic arguments.
        base_type_exists = scope_handler.current_scope.has_symbol(self.without_generics())
        this_type_exists = scope_handler.current_scope.has_symbol(self)
        generic_arguments = Seq(self.parts[-1].generic_arguments.arguments.copy())

        # Check the namespace exists.  todo: identify which part of the namespace is invalid + "similar" in parent scope
        namespace = Seq(self.parts).filter_to_type(IdentifierAst).value
        if not scope_handler.get_namespaced_scope(namespace):
            raise SemanticErrors.UNKNOWN_IDENTIFIER(namespace[-1], [], "namespace")

        # Check if the base type exists.
        if not base_type_exists:
            scope = scope_handler.get_namespaced_scope(namespace) if namespace else scope_handler.current_scope
            types = Seq(scope.all_symbols()).filter_to_type(TypeSymbol).map(lambda s: s.name).map(lambda t: t.parts[-1].to_identifier().value).value
            raise SemanticErrors.UNKNOWN_IDENTIFIER(self.parts[-1].to_identifier(), types, "type")

        elif not this_type_exists and generic_arguments:
            # Get the symbol and scope for the base type.
            base_type_symbol = scope_handler.current_scope.get_symbol(self.without_generics())
            base_type_scope  = base_type_symbol.associated_scope

            # Convert all anonymous generic arguments to named generic arguments (in the type being instantiated).
            convert_generic_arguments_to_named(
                generic_arguments=generic_arguments,
                generic_parameters=Seq(base_type_symbol.type.generic_parameters.parameters))

            if self.without_generics() != CommonTypes.tuple([]):
                self.parts[-1].generic_arguments = GenericArgumentGroupAst.from_list(generic_arguments.value)
                self.parts[-1].generic_arguments = GenericArgumentGroupAst.from_dict(infer_generics_types(
                    self,
                    Seq(base_type_symbol.type.generic_parameters.parameters).map(lambda p: p.identifier).value,
                    Seq(self.parts[-1].generic_arguments.arguments).map(lambda a: (a.identifier, a.type)).dict(),
                    {}, {}, scope_handler))

            self.parts[-1].generic_arguments.do_semantic_analysis(scope_handler, **kwargs)

            # Create a new scope and symbol for the generic version of the type.
            this_type_scope_name = copy.deepcopy(base_type_scope._scope_name)
            this_type_scope_name.parts[-1].generic_arguments.arguments = generic_arguments.value
            this_type_scope = Scope(this_type_scope_name, scope_handler.global_scope)
            this_type_scope._sup_scopes = base_type_scope._sup_scopes
            this_type_scope._symbol_table = copy.deepcopy(base_type_scope._symbol_table)
            this_type_cls_ast = copy.deepcopy(base_type_symbol.type)

            # Add the new scope and symbol to the correct parent scope.
            scope_handler.global_scope._children_scopes.append(this_type_scope)
            scope_handler.global_scope.add_symbol(TypeSymbol(name=self, type=this_type_cls_ast, associated_scope=this_type_scope))

            # Add the generic arguments mapping to the correct types into the new symbol table.
            if self.without_generics() != CommonTypes.tuple([]):
                for generic_argument in generic_arguments:
                    this_type_scope.add_symbol(TypeSymbol(
                        name=generic_argument.identifier,
                        type=scope_handler.current_scope.get_symbol(generic_argument.type).type,
                        associated_scope=scope_handler.current_scope.get_symbol(generic_argument.type).associated_scope))

                    for attribute_symbol in Seq(this_type_scope.all_symbols()).filter_to_type(VariableSymbol):
                        attribute_symbol.type.substitute_generics(generic_argument.identifier, generic_argument.type)

                    for attribute in this_type_cls_ast.body.members:
                        attribute.type_declaration.substitute_generics(generic_argument.identifier, generic_argument.type)

        elif base_type_exists and (base_type_symbol := scope_handler.current_scope.get_symbol(self.without_generics())).type:

            convert_generic_arguments_to_named(
                generic_arguments=generic_arguments,
                generic_parameters=Seq(base_type_symbol.type.generic_parameters.parameters))

            if self.without_generics() != CommonTypes.tuple([]):
                self.parts[-1].generic_arguments = GenericArgumentGroupAst.from_list(generic_arguments.value)
                self.parts[-1].generic_arguments = GenericArgumentGroupAst.from_dict(infer_generics_types(
                    self,
                    Seq(base_type_symbol.type.generic_parameters.parameters).map(lambda p: p.identifier).value,
                    Seq(self.parts[-1].generic_arguments.arguments).map(lambda a: (a.identifier, a.type)).dict(),
                    {}, {}, scope_handler))

    def __iter__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import IdentifierAst

        # Iterate the parts, and recursively the parts of generic parameters
        def iterate(type_single: TypeAst):
            namespace_parts = Seq(type_single.parts).filter(lambda p: isinstance(p, IdentifierAst)).value
            non_namespace_parts = Seq(type_single.parts).filter(lambda p: not isinstance(p, IdentifierAst)).value

            for part in non_namespace_parts:
                yield TypeAst(part.pos, [*namespace_parts, part])
                for g in part.generic_arguments.arguments:
                    yield from iterate(g.type)

        return iterate(self)

    def without_generics(self) -> TypeAst:
        from SPPCompiler.SemanticAnalysis.ASTs import GenericIdentifierAst

        parts = []
        for part in self.parts:
            parts.append(GenericIdentifierAst(part.pos, part.value, None) if isinstance(part, GenericIdentifierAst) else part)
        return TypeAst(self.pos, parts)

    def __eq__(self, that):
        # Check both ASTs are the same type and have the same parts.
        return isinstance(that, TypeAst) and self.parts == that.parts

    def symbolic_eq(self, that, this_scope: Scope, that_scope: Optional[Scope] = None) -> bool:
        # Special cases for union types.  todo: re-check this
        if that.without_generics() == CommonTypes.var([]):
            for generic_argument in that.parts[-1].generic_arguments.arguments:
                if generic_argument.type.symbolic_eq(self, this_scope, that_scope):
                    return True

        # Allows for generics and aliases to match base types etc.
        that_scope = that_scope or this_scope
        this_type = this_scope.get_symbol(self).type
        that_type = that_scope.get_symbol(that).type
        return this_type is that_type

    def __hash__(self):
        return int.from_bytes(hashlib.md5("".join([str(p) for p in self.parts]).encode()).digest())

    def __json__(self) -> str:
        printer = AstPrinter()
        return self.print(printer)


__all__ = ["TypeAst"]

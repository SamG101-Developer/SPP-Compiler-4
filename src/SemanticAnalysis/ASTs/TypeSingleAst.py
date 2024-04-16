from __future__ import annotations

import copy, difflib, hashlib
from dataclasses import dataclass
from typing import List, Optional

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError
from src.SemanticAnalysis.Utils.Scopes import Scope, ScopeHandler
from src.SemanticAnalysis.Utils.Symbols import TypeSymbol, VariableSymbol
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from src.SemanticAnalysis.ASTs.Meta.AstUtils import AstUtils

from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst
from src.SemanticAnalysis.ASTs.GenericArgumentGroupAst import GenericArgumentGroupAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypePartAst import TypePartAst

from src.Utils.Sequence import Seq


@dataclass
class TypeSingleAst(Ast, SemanticAnalyser):
    """
    A TypeSingleIdentifier is a single type, ie `std.Vec[T]`. It is made up of a sequence of TypePartAst's, which can be
    either IdentifierAsts (namespace parts), GenericIdentifierAsts (type parts), or TokenAsts (numbers for tuple
    access).

    Attributes:
        - parts: The parts that make up the type identifier.
    """

    parts: List[TypePartAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypeSingleAst.
        s = ""
        s += f"{Seq(self.parts).print(printer, '.')}"
        return s

    def substitute_generics(self, from_ty: TypeSingleAst, to_ty: TypeSingleAst) -> TypeSingleAst:
        # Substitute the generic type "from_ty" with "to_ty" in the type identifier (recursively).
        namespace_parts = Seq(self.parts).filter(lambda p: isinstance(p, IdentifierAst)).value
        self._substitute_generics(from_ty, to_ty)
        return self

    def _substitute_generics(self, from_ty: TypeSingleAst, to_ty: TypeSingleAst) -> TypeSingleAst:
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

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        base_type_exists = scope_handler.current_scope.has_symbol(self.without_generics())
        this_type_exists = scope_handler.current_scope.has_symbol(self)
        generic_arguments = Seq(self.parts[-1].generic_arguments.arguments)

        # Check namespace exists
        type_namespace = Seq(self.parts).filter(lambda p: isinstance(p, IdentifierAst)).value
        type_namespace_string = Seq(type_namespace).map(lambda p: p.value).join(".")
        if not (namespace_scope := scope_handler.get_namespaced_scope(type_namespace)):
            exception = SemanticError(f"Namespace '{type_namespace_string}' is not defined:")
            exception.add_error(self.pos, f"Namespace '{type_namespace_string}' used here.")
            raise exception

        if not base_type_exists:
            scope = namespace_scope if type_namespace else scope_handler.current_scope

            all_symbols = Seq(scope.all_symbols()).filter(lambda s: isinstance(s, TypeSymbol))
            closest_match = difflib.get_close_matches(str(self), all_symbols.map(lambda s: str(s.name)).value, n=1)
            closest_match = f" Did you mean '{type_namespace_string}.{closest_match[0]}'?" if closest_match else ""

            exception = SemanticError(f"Type '{self}' is not defined:")
            exception.add_error(self.pos, f"Type '{self}' used here.{closest_match}")
            raise exception

        elif not this_type_exists and self.parts[-1].generic_arguments.arguments:
            # Get the symbol for the base type without any generic arguments, and get its associated scope. The parent
            # scope of the type is needed as this is the scope that holds all the types.
            type_sym = scope_handler.current_scope.get_symbol(self.without_generics())
            type_scope = type_sym.associated_scope

            # For each generic parameter, set its type to the corresponding generic argument.
            self.parts[-1].generic_arguments.do_semantic_analysis(scope_handler, **kwargs)
            all_generic_arguments = AstUtils.verify_generic_arguments(
                generic_parameters=Seq(type_sym.type.generic_parameters.parameters),
                inferred_generic_arguments=Seq([]),
                generic_arguments=generic_arguments,
                obj_definition=type_sym.type,
                usage=self,
                scope_handler=scope_handler,
                **kwargs) if self.without_generics() != CommonTypes.tuple([]) else Seq(self.parts[-1].generic_arguments.arguments)

            # If the type is a tuple, then its generic arguments are a tuple (variadic) etc etc, so jump into the
            # arguments already
            if self.without_generics() == CommonTypes.tuple([]):
                Seq(self.parts[-1].generic_arguments.arguments).for_each(lambda g: g.type.do_semantic_analysis(scope_handler, **kwargs))
            else:
                all_generic_arguments.for_each(lambda g: g.type.do_semantic_analysis(scope_handler, **kwargs))

            # Copy the scope name, and create a new scope whose name includes the generic arguments. This allows for
            # multiple types with the same name, but different generic arguments, to exist in the same scope.
            modified_type_scope_name = copy.deepcopy(type_scope._scope_name)
            modified_type_scope_name.parts[-1].generic_arguments.arguments = all_generic_arguments.value
            modified_type_scope = Scope(modified_type_scope_name, scope_handler.global_scope)
            modified_type_scope._sup_scopes = type_scope._sup_scopes
            modified_type_scope._symbol_table = copy.deepcopy(type_scope._symbol_table)

            # Copy the type, and substitute the attribute types with the generic arguments.
            modified_type = copy.deepcopy(type_sym.type)

            # Inject the type into the parent scope
            scope_handler.global_scope._children_scopes.append(modified_type_scope)
            scope_handler.global_scope.add_symbol(TypeSymbol(self, modified_type, modified_type_scope))

            # Check each generic argument is a valid type
            for generic_argument in all_generic_arguments:
                generic_argument.type.do_semantic_analysis(scope_handler, **kwargs)
                type_sym = scope_handler.current_scope.get_symbol(generic_argument.type)

                if self.without_generics() != CommonTypes.tuple([]):  # todo
                    modified_type_scope.add_symbol(TypeSymbol(generic_argument.identifier, type_sym.type))
                    modified_type_scope.get_symbol(generic_argument.identifier).associated_scope = type_sym.associated_scope

                    for attribute in Seq(modified_type_scope.all_symbols()).filter_to_type(VariableSymbol):
                        attribute.type.substitute_generics(generic_argument.identifier, generic_argument.type)

                    for attribute in modified_type.body.members:
                        attribute.type_declaration.substitute_generics(generic_argument.identifier, generic_argument.type)

    def __iter__(self):
        # Iterate the parts, and recursively the parts of generic parameters
        def iterate(type_single: TypeSingleAst):
            namespace_parts = Seq(type_single.parts).filter(lambda p: isinstance(p, IdentifierAst)).value
            non_namespace_parts = Seq(type_single.parts).filter(lambda p: not isinstance(p, IdentifierAst)).value

            for part in non_namespace_parts:
                yield TypeSingleAst(part.pos, [*namespace_parts, part])
                for g in part.generic_arguments.arguments:
                    yield from iterate(g.type)

        return iterate(self)

    def without_generics(self) -> TypeSingleAst:
        parts = []
        for part in self.parts:
            parts.append(GenericIdentifierAst(part.pos, part.value, GenericArgumentGroupAst(part.pos, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))) if isinstance(part, GenericIdentifierAst) else part)
        return TypeSingleAst(self.pos, parts)

    def __eq__(self, that):
        # Check both ASTs are the same type and have the same parts.
        return isinstance(that, TypeSingleAst) and self.parts == that.parts

    def symbolic_eq(self, that, this_scope: Scope, that_scope: Optional[Scope] = None) -> bool:
        # Special cases for union types.
        if that.without_generics() == CommonTypes.union([]):
            for generic_argument in that.parts[-1].generic_arguments.arguments:
                if generic_argument.type.symbolic_eq(self, this_scope, that_scope):
                    return True

        # Allows for generics and aliases to match base types etc.
        that_scope = that_scope or this_scope
        this_type = this_scope.get_symbol(self).type
        that_type = that_scope.get_symbol(that).type
        return this_type == that_type

    def __hash__(self):
        return int.from_bytes(hashlib.md5("".join([str(p) for p in self.parts]).encode()).digest())

    def __json__(self) -> str:
        printer = AstPrinter()
        return self.print(printer)


__all__ = ["TypeSingleAst"]

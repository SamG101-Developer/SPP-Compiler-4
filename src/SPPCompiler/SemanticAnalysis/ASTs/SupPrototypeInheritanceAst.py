import json
from dataclasses import dataclass, field
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SupScopeLoader
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import SupInheritanceIdentifier, get_owner_type_of_sup_block, check_for_conflicting_methods, FunctionConflictCheckType
from SPPCompiler.SemanticAnalysis.ASTs.SupPrototypeNormalAst import SupPrototypeNormalAst
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class SupPrototypeInheritanceAst(SupPrototypeNormalAst, SupScopeLoader):
    """
    The SupPrototypeInheritanceAst node represents a superimposition prototype for a class to be superimposed over
    another class.

    Attributes:
        ext_keyword: The "ext" keyword token.
        super_class: The superclass of the superimposition.
    """

    ext_keyword: "TokenAst"
    super_class: "TypeAst"

    _scope: Optional["Scope"] = field(default=None, kw_only=True, repr=False, init=False)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the SupPrototypeInheritanceAst.
        s = ""
        s += f"{self.sup_keyword.print(printer)} "
        s += f"{self.identifier.print(printer)}"
        s += f"{self.generic_parameters.print(printer)} " if self.generic_parameters else ""
        s += f"{self.ext_keyword.print(printer)} "
        s += f"{self.super_class.print(printer)} "
        s += f" {self.where_block.print(printer)} " if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    def pre_process(self, context: "ModulePrototypeAst") -> None:
        # Don't preprocess converted function classes, because they will infinitely generate new ones inside.
        # TODO: Change to std::FunRef, std::FunMut, std::FunMov
        if self.super_class.types[-1].value in ["FunRef", "FunMut", "FunMov"]:
            return

        # Use the SupPrototypeNormalAst's implementation.
        super().pre_process(context)

    def generate(self, scope_handler: ScopeHandler) -> None:
        # Create a new scope.
        scope_name = SupInheritanceIdentifier(this_class=self.identifier, super_class=self.super_class)
        scope_handler.into_new_scope(scope_name)
        self._scope = scope_handler.current_scope

        # Generate the body members.
        Seq(self.generic_parameters.parameters).for_each(lambda p: scope_handler.current_scope.add_symbol(TypeSymbol(name=p.identifier.types[-1], type=None, is_generic=True)))
        Seq(self.body.members).for_each(lambda m: m.generate(scope_handler))

        # Exit the new scope.
        scope_handler.exit_cur_scope()

    def load_sup_scopes_gen(self, scope_handler: ScopeHandler) -> None:
        scope_handler.move_to_next_scope()

        # Get the class symbol and associated scope.
        cls_symbol = scope_handler.current_scope.get_symbol(self.identifier.without_generics())

        # Can't superimpose over a generic type.
        super_class_symbol = scope_handler.current_scope.get_symbol(self.super_class)
        if super_class_symbol and super_class_symbol.is_generic:
            raise SemanticErrors.SUPERIMPOSITION_ONTO_GENERIC(self.super_class, super_class_symbol.name)

        # Register the superclass scope against the class scope (allows state access to this type).
        self.super_class.do_semantic_analysis(scope_handler)
        super_class_symbol = scope_handler.current_scope.get_symbol(self.super_class)
        cls_symbol.associated_scope._sup_scopes.append((super_class_symbol.associated_scope, super_class_symbol.type))

        # Mark the class-type as "copyable" if the superclass std::Copy.
        if self.super_class.symbolic_eq(CommonTypes.copy(), scope_handler.current_scope):
            cls_symbol.is_copyable = True

        # Load the superimposition scopes for the members.
        Seq(self.body.members).for_each(lambda m: m.load_sup_scopes_gen(scope_handler))

        scope_handler.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        scope_handler.move_to_next_scope()

        # Analyse the generic type parameters and where block. This will load the generics into the current scope, and
        # ensure all their constraints are valid.
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.where_block.do_semantic_analysis(scope_handler, **kwargs)

        # Make sure every generic parameter is present in the identifier, otherwise it has no way to be inferred.
        for generic_parameter in Seq(self.generic_parameters.parameters).map(lambda p: p.identifier):
            if not self.identifier.contains_generic(generic_parameter):
                if CommonTypes.is_function_type(self.super_class) and self.super_class.contains_generic(generic_parameter):
                    continue
                raise SemanticErrors.UNCONSTRAINED_GENERIC_PARAMETER(self, generic_parameter)

        # Ensure the identifier and superclass exist, then analyse the body.
        # print("SUPERCLASS", self.super_class, self.generic_parameters, scope_handler.current_scope.parent)

        self.identifier.do_semantic_analysis(scope_handler, **kwargs)
        self.super_class.do_semantic_analysis(scope_handler, **kwargs)
        self.body.do_semantic_analysis(scope_handler, inline=True, **kwargs)

        # Get superclass symbol/scope information.
        super_class_symbol = scope_handler.current_scope.get_symbol(self.super_class)
        super_class_scope = super_class_symbol.associated_scope

        # Ensure every member exists on the superclass.
        for this_member in Seq(self.body.members).filter_to_type(SupPrototypeInheritanceAst):

            new_function = this_member.body.members[-1]
            overridden_function = check_for_conflicting_methods(super_class_scope, scope_handler, new_function, FunctionConflictCheckType.InvalidOverride)

            if not overridden_function:
                raise SemanticErrors.INVALID_SUPERIMPOSITION_MEMBER(new_function, self.super_class)

        scope_handler.exit_cur_scope()

        # Todo: Check Tree:
        #  - Check there are no direct duplicate sup super-classes
        #  - Check there are no loops in the inheritance tree


__all__ = ["SupPrototypeInheritanceAst"]

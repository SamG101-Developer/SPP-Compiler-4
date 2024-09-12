import json
from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SupScopeLoader
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import SupInheritanceIdentifier, get_owner_type_of_sup_block
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
        super_class: The superclass of the superimposition.
        on_keyword: The "on" keyword token.
    """

    super_class: "TypeAst"
    on_keyword: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the SupPrototypeInheritanceAst.
        s = ""
        s += f"{self.sup_keyword.print(printer)}"
        s += f"{self.generic_parameters.print(printer)} " if self.generic_parameters else ""
        s += f"{self.super_class.print(printer)} "
        s += f"{self.on_keyword.print(printer)}"
        s += f"{self.identifier.print(printer)}"
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

        # Generate the body members (prototype), and register the generic parameters types.
        Seq(self.generic_parameters.parameters).for_each(lambda p: scope_handler.current_scope.add_symbol(TypeSymbol(name=p.identifier.types[-1], type=None, is_generic=True)))
        Seq(self.body.members).for_each(lambda m: m.generate(scope_handler))

        # Exit the new scope.
        scope_handler.exit_cur_scope()

    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        scope_handler.move_to_next_scope()
        self_symbol = get_owner_type_of_sup_block(self.identifier, scope_handler)

        # Register the "Self" type and analyse the identifier.
        self_symbol and scope_handler.current_scope.add_symbol(TypeSymbol(
            name=CommonTypes.self().types[-1],
            type=self_symbol.type,
            associated_scope=self_symbol.associated_scope))

        # Can't superimpose over a generic type.
        cls_symbol = scope_handler.current_scope.get_symbol(self.identifier.without_generics())
        if cls_symbol.is_generic:
            raise SemanticErrors.SUPERIMPOSITION_ONTO_GENERIC(self.identifier.without_generics(), cls_symbol.name)

        # Add the superimposition scope to the class scope.
        cls_scope = cls_symbol.associated_scope
        cls_scope._sup_scopes.append((scope_handler.current_scope, self))

        # Load the sup-scopes for methods defined over the "sup" block.
        Seq(self.body.members).for_each(lambda m: m.load_sup_scopes(scope_handler))

        scope_handler.exit_cur_scope()

    def load_sup_scopes_gen(self, scope_handler: ScopeHandler) -> None:
        scope_handler.move_to_next_scope()

        # Get the class symbol and associated scope.
        cls_symbol = scope_handler.current_scope.get_symbol(self.identifier.without_generics())
        cls_scope = cls_symbol.associated_scope

        # Can't superimpose over a generic type.
        if (super_class_symbol := scope_handler.current_scope.get_symbol(self.super_class)) and super_class_symbol.is_generic:
            raise SemanticErrors.SUPERIMPOSITION_ONTO_GENERIC(self.super_class, super_class_symbol.name)

        # Register the superclass scope against the class scope.
        self.super_class.do_semantic_analysis(scope_handler)
        cls_scope._sup_scopes.append((scope_handler.current_scope.get_symbol(self.super_class).associated_scope, self))

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
        self.super_class.do_semantic_analysis(scope_handler, **kwargs)
        self.identifier.do_semantic_analysis(scope_handler, **kwargs)
        self.body.do_semantic_analysis(scope_handler, inline=True, **kwargs)

        # Check all members on this superimposition are present on the superclass.
        # super_class_symbol = scope_handler.current_scope.get_symbol(self.super_class.without_generics())
        # super_class_scope = super_class_symbol.associated_scope
        # for member in Seq(self.body.members).filter_to_type(SupPrototypeInheritanceAst):
        #     for m in member.body.members:
        #         if not check_for_conflicting_methods(super_class_scope, scope_handler, m, FunctionConflictCheckType.InvalidOverride):
        #             raise SemanticErrors.INVALID_SUPERIMPOSITION_MEMBER(m, self.super_class)

        # super_class_symbol = scope_handler.current_scope.get_symbol(self.super_class)
        # super_class_scope = super_class_symbol.associated_scope
        # super_class_implementations = Seq(super_class_scope._sup_scopes)
        #
        # # Todo: as compiler develops, add the sup-typedefs here
        # super_class_members = super_class_implementations.map(lambda x: x[1].body.members).flat().filter_to_type(SupPrototypeInheritanceAst)
        # this_class_members  = Seq(self.body.members).filter_to_type(SupPrototypeInheritanceAst)
        #
        # # Compare the members to ensure the superclass contains the members.
        # for this_member in this_class_members:
        #     matched = False
        #
        #     for that_member in super_class_members:
        #
        #         if any([
        #                 this_member.generic_parameters != that_member.generic_parameters,
        #                 this_member.where_block != that_member.where_block,
        #                 this_member.identifier != that_member.identifier,
        #                 this_member.super_class.types[-1].value != that_member.super_class.types[-1].value]):
        #             continue
        #
        #         # The superclass is more complex, as the "Self" argument and return type can be different.
        #         matched = matching_function_types(this_member, that_member, scope_handler.current_scope, super_class_scope)
        #         if matched:
        #             break
        #
        #     if not matched:
        #         raise SemanticErrors.INVALID_SUPERIMPOSITION_MEMBER(this_member, self.super_class)
        #
        scope_handler.exit_cur_scope()
        #
        # # TODO : check there are no direct duplicate sup super-classes


__all__ = ["SupPrototypeInheritanceAst"]

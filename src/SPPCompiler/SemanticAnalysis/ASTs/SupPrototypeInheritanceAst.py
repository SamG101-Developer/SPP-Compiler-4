import json
from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SupScopeLoader
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
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
        s += f"{self.generic_parameters.print(printer)}" if self.generic_parameters else ""
        s += f"{self.super_class.print(printer)} "
        s += f"{self.on_keyword.print(printer)}"
        s += f"{self.identifier.print(printer)}"
        s += f" {self.where_block.print(printer)}" if self.where_block else ""
        s += f"{self.body.print(printer)}"
        return s

    def pre_process(self, context: "ModulePrototypeAst") -> None:
        # Don't preprocess converted function classes, because they will infinitely generate new ones inside.
        # TODO: Change to std::FunRef, std::FunMut, std::FunMov
        if self.super_class.parts[-1].value in ["FunRef", "FunMut", "FunMov"]:
            return

        # Substitute the "Self" type to the identifier of the class.
        Seq(self.super_class.parts[-1].generic_arguments.arguments).for_each(lambda a: a.type.substitute_generics(CommonTypes.self(), self.identifier))

        # Use the SupPrototypeNormalAst's implementation.
        super().pre_process(context)

    def generate(self, scope_handler: ScopeHandler) -> None:
        # Create a new scope, and add the "Self" type to the scope.
        scope_handler.into_new_scope(f"{self.identifier}#SUP-{self.super_class}")

        # Generate the body members (prototype), and register the generic parameters types.
        Seq(self.generic_parameters.parameters).for_each(lambda p: scope_handler.current_scope.add_symbol(TypeSymbol(name=p.identifier, type=None)))
        Seq(self.body.members).for_each(lambda m: m.generate(scope_handler))

        # Exit the new scope.
        scope_handler.exit_cur_scope()

    def load_sup_scopes(self, scope_handler: ScopeHandler) -> None:
        scope_handler.move_to_next_scope()

        # Register the "Self" type and analyse the identifier.
        self.identifier.do_semantic_analysis(scope_handler)
        scope_handler.current_scope.add_symbol(TypeSymbol(
            name=CommonTypes.self(),
            type=scope_handler.current_scope.get_symbol(self.identifier).type))

        # Register the class's generics into the sup-block.
        for generic_argument in self.identifier.parts[-1].generic_arguments.arguments:
            generic_argument_type = scope_handler.current_scope.get_symbol(generic_argument.type).type
            generic_argument_name = generic_argument.identifier
            scope_handler.current_scope.add_symbol(TypeSymbol(name=generic_argument_name, type=generic_argument_type))

        # Add the superimposition scope to the class scope.
        cls_scope = scope_handler.current_scope.get_symbol(self.identifier).associated_scope
        cls_scope._sup_scopes.append((scope_handler.current_scope, self))

        if self.super_class.types[-1].value not in ["FunRef", "FunMut", "FunMov"]:
            self.super_class.do_semantic_analysis(scope_handler)
            cls_scope._sup_scopes.append((scope_handler.current_scope.get_symbol(self.super_class).associated_scope, self))

        # Skip internal functions scopes.
        Seq(self.body.members).for_each(lambda m: m.load_sup_scopes(scope_handler))
        scope_handler.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        scope_handler.move_to_next_scope()

        # Analyse the generic type parameters and where block. This will load the generics into the current scope, and
        # ensure all their constraints are valid.
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.where_block.do_semantic_analysis(scope_handler, **kwargs)

        # Make sure every generic parameter is present in the identifier or superclass; otherwise it has no way to be
        # inferred.
        for generic_parameter in Seq(self.generic_parameters.parameters).map(lambda p: p.identifier):
            if not self.identifier.contains_generic(generic_parameter) and not self.super_class.contains_generic(generic_parameter):
                raise SemanticErrors.UNCONSTRAINED_GENERIC_PARAMETER(self, generic_parameter)

        # Make sure the superclass, and the identifier (the type being superimposed over), exists. If it does, analyse
        # each member of the body.
        self.super_class.do_semantic_analysis(scope_handler, **kwargs)
        self.identifier.do_semantic_analysis(scope_handler, **kwargs)
        self.body.do_semantic_analysis(scope_handler, inline=True, **kwargs)

        # Check all members on this superimposition are present on the superclass.
        super_class_scope = scope_handler.current_scope.get_symbol(self.super_class).associated_scope
        super_class_implementations = Seq(super_class_scope._normal_sup_scopes)

        # todo: as compiler develops, add the sup-typedefs here
        super_class_members = super_class_implementations.map(lambda x: x[1].body.members).flat().filter_to_type(SupPrototypeInheritanceAst)
        this_class_members  = Seq(self.body.members).filter_to_type(SupPrototypeInheritanceAst)

        # Compare the members to ensure the superclass contains the members.
        for this_member in this_class_members:
            matched = False

            for that_member in super_class_members:

                # Check basic attributes, including the function type (FunRef vs FunMut for example).
                if this_member.generic_parameters != that_member.generic_parameters: continue
                if this_member.identifier != that_member.identifier: continue
                if this_member.where_block != that_member.where_block: continue
                if this_member.super_class.parts[-1].value != that_member.super_class.parts[-1].value: continue

                # The superclass is more complex, as the "Self" argument and return type can be different.
                this_member_has_self_parameter = this_member.body.members[0].parameters.get_self() is not None
                that_member_has_self_parameter = that_member.body.members[0].parameters.get_self() is not None
                t1 = this_member.super_class.parts[-1].generic_arguments.arguments[-1].type.parts[-1].generic_arguments.arguments[this_member_has_self_parameter:]
                t2 = that_member.super_class.parts[-1].generic_arguments.arguments[-1].type.parts[-1].generic_arguments.arguments[that_member_has_self_parameter:]
                c5 = all(tt1.type.symbolic_eq(tt2.type, scope_handler.current_scope, super_class_scope) for tt1, tt2 in zip(t1, t2))

                matched = c5
                if matched:
                    break

            if not matched:
                raise SemanticErrors.INVALID_SUPERIMPOSITION_MEMBER(this_member, self.super_class)

        scope_handler.exit_cur_scope()

        # TODO : check there are no direct duplicate sup super-classes


__all__ = ["SupPrototypeInheritanceAst"]

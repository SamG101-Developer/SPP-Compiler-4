from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.SupPrototypeNormalAst import SupPrototypeNormalAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class SupPrototypeInheritanceAst(SupPrototypeNormalAst):
    """
    The SupPrototypeInheritanceAst node represents a superimposition prototype for a class to be superimposed over
    another class.

    Attributes:
        - super_class: The superclass of the superimposition.
        - on_keyword: The "on" keyword token.
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
        if self.super_class.parts[-1].value in ["FunRef", "FunMut", "FunMov"]:
            return

        # Use the SupPrototypeNormalAst's implementation.
        super().pre_process(context)

    def generate(self, scope_handler: ScopeHandler) -> None:
        # Create a new scope, and add the "Self" type to the scope.
        scope_handler.into_new_scope(self.identifier.parts[-1].value + f"#SUP-{self.super_class}")
        scope_handler.current_scope.add_symbol(TypeSymbol(
            name=CommonTypes.self(),
            type=scope_handler.current_scope.get_symbol(self.identifier).type))

        # Generate the body members (prototype), and register the generic parameters types.
        Seq(self.body.members).for_each(lambda m: m.generate(scope_handler))
        Seq(self.generic_parameters.parameters).for_each(lambda p: scope_handler.current_scope.add_symbol(TypeSymbol(name=p.identifier, type=None)))

        # Add the superimposition scope to the class scope.
        cls_scope = scope_handler.current_scope.get_symbol(self.identifier.without_generics()).associated_scope
        cls_scope._sup_scopes.append((scope_handler.current_scope, self))

        # Exit the new scope.
        scope_handler.exit_cur_scope()

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        scope_handler.move_to_next_scope()

        # Analyse the generic type parameters and where block. This will load the generics into the current scope, and
        # ensure all their constraints are valid.
        self.generic_parameters.do_semantic_analysis(scope_handler, **kwargs)
        self.where_block.do_semantic_analysis(scope_handler, **kwargs)

        # Make sure the superclass, and the identifier (the type being superimposed over), exists. If it does, analyse
        # each member of the body.
        self.super_class.do_semantic_analysis(scope_handler, **kwargs)
        self.identifier.do_semantic_analysis(scope_handler, **kwargs)
        self.body.do_semantic_analysis(scope_handler, inline=True, **kwargs)

        scope_handler.exit_cur_scope()


__all__ = ["SupPrototypeInheritanceAst"]

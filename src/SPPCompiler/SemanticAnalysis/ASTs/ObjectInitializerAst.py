from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType, infer_generics_types
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import convert_generic_arguments_to_named
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ObjectInitializerAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The ObjectInitializerAst node represents the construction of an object of a class type, with the given arguments.

    Attributes:
        class_type: The type of the object to be constructed.
        arguments: The arguments to be given to the object's constructor.
    """

    class_type: "TypeAst"
    arguments: "ObjectInitializerArgumentGroupAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ObjectInitializerAst.
        s = ""
        s += f"{self.class_type.print(printer)}"
        s += f"{self.arguments.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import GenericArgumentGroupAst

        # Ensure the base type (no generics) exists, and is not a generic type, as these cannot be instantiated.
        # The non-generic version of the type is checked, as the generics can be changed by inferred generic from
        # arguments, so the complete type, with generics, is analysed later.
        self.class_type.do_semantic_analysis(scope_handler, **(kwargs | {"is-init": True}))
        type_symbol = scope_handler.current_scope.get_symbol(self.class_type)
        if not type_symbol.type:
            raise SemanticErrors.NON_INSTANTIABLE_TYPE(self.class_type, type_symbol)

        kwargs["attributes"] = Seq(type_symbol.type.body.members).map(lambda a: a.identifier)
        kwargs["class-type"] = self.class_type

        # The pre-analysis ensures all the arguments that should be present are, and handles the default/sup special
        # arguments.
        self.arguments.do_semantic_pre_analysis(scope_handler, **kwargs)

        # Convert all anonymous generic arguments to named generic arguments (in the type being instantiated).
        self.class_type.parts[-1].generic_arguments.arguments = convert_generic_arguments_to_named(
            generic_arguments=Seq(self.class_type.parts[-1].generic_arguments.arguments),
            generic_parameters=Seq(type_symbol.type.generic_parameters.parameters)).value

        # Infer all the generic from arguments, and analyse the new generic-complete type. This type is required to be
        # analysed so that the type-checking of arguments can be done.
        base_type_symbol = scope_handler.current_scope.get_symbol(self.class_type.without_generics())
        self.class_type.parts[-1].generic_arguments = GenericArgumentGroupAst.from_dict(infer_generics_types(
            self,
            Seq(type_symbol.type.generic_parameters.get_req()).map(lambda p: p.identifier).value,
            Seq(self.class_type.parts[-1].generic_arguments.arguments).map(lambda a: (a.identifier, a.type)).dict(),
            Seq(self.arguments.arguments).map(lambda a: (a.identifier, self.arguments.get_argument_value(a).infer_type(scope_handler, **kwargs).type)).dict(),
            Seq(base_type_symbol.type.body.members).map(lambda a: (a.identifier, a.type_declaration)).dict(),
            scope_handler))
        self.class_type.do_semantic_analysis(scope_handler, **kwargs)

        # Type-check the arguments of the object initializer.
        self.arguments.do_semantic_analysis(scope_handler, **kwargs)

        # Substitution for sup-scopes?

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # The returning type of the object initializer is the type of the object being constructed.
        return InferredType(convention=ConventionMovAst, type=self.class_type)


__all__ = ["ObjectInitializerAst"]

from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
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
        # Load generic information for type-analysis.
        base_type_symbol = scope_handler.current_scope.get_symbol(self.class_type.without_generics())
        if not base_type_symbol:
            raise SemanticErrors.UNKNOWN_IDENTIFIER(self.class_type, [], "type")

        generic_infer_from = Seq(self.arguments.arguments).map(lambda a: (a.identifier, self.arguments.get_argument_value(a).infer_type(scope_handler, **kwargs).type)).dict()
        generic_map_to = Seq(base_type_symbol.type.body.members).map(lambda a: (a.identifier, a.type_declaration)).dict()

        # Analyse the type, and ensure that it isn't a generic type being instantiated.
        self.class_type.do_semantic_analysis(scope_handler, generic_infer_from, generic_map_to, **kwargs)
        type_symbol = scope_handler.current_scope.get_symbol(self.class_type)
        if type_symbol.is_generic:
            raise SemanticErrors.NON_INSTANTIABLE_TYPE(self.class_type, type_symbol)

        # The pre-analysis ensures all the arguments that should be present are, and handles the default/sup special
        # arguments.
        kwargs["attributes"] = Seq(type_symbol.type.body.members).map(lambda a: a.identifier)
        kwargs["class-type"] = self.class_type
        self.arguments.do_semantic_pre_analysis(scope_handler, **kwargs)

        # Type-check the arguments of the object initializer.
        self.arguments.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # The returning type of the object initializer is the type of the object being constructed.
        return InferredType(convention=ConventionMovAst, type=self.class_type)


__all__ = ["ObjectInitializerAst"]

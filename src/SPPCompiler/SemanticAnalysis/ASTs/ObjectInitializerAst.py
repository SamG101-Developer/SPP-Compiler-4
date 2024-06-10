from dataclasses import dataclass, field
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import infer_generics_types
from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
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

        _modified_type: The type of the object to be constructed, with generic arguments inferred.
    """

    class_type: "TypeAst"
    arguments: "ObjectInitializerArgumentGroupAst"
    _modified_type: Optional["TypeAst"] = field(default=None, init=False)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ObjectInitializerAst.
        s = ""
        s += f"{self.class_type.print(printer)}"
        s += f"{self.arguments.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs import (
            GenericArgumentNamedAst, GenericArgumentNormalAst, TokenAst, GenericArgumentGroupAst)

        # Ensure the base type (no generics) exists, and is not a generic type, as these cannot be instantiated.
        # The non-generic version of the type is checked, as the generics can be changed by inferred generic from
        # arguments, so the complete type, with generics, is analysed later.
        self.class_type.without_generics().do_semantic_analysis(scope_handler, **kwargs)
        type_symbol = scope_handler.current_scope.get_symbol(self.class_type)
        if not type_symbol.type:
            raise SemanticErrors.NON_INSTANTIABLE_TYPE(self.class_type, type_symbol)
        kwargs["attributes"] = Seq(type_symbol.type.body.members).map(lambda a: a.identifier)
        kwargs["class-type"] = self.class_type

        # The pre-analysis ensures all the arguments that should be present are, and handles the default/sup special
        # arguments.
        self.arguments.do_semantic_pre_analysis(scope_handler, **kwargs)

        # Convert all anonymous generic arguments to named generic arguments (in the type being instantiated).
        generic_parameter_identifiers = Seq(type_symbol.type.generic_parameters.parameters).map(lambda p: p.identifier).value.copy()
        generic_arguments = Seq(self.class_type.parts[-1].generic_arguments.arguments)
        for generic_argument in generic_arguments.filter_to_type(GenericArgumentNormalAst):
            new_argument = GenericArgumentNamedAst(generic_argument.pos, generic_parameter_identifiers.pop(0).parts[-1].to_identifier(), TokenAst.dummy(TokenType.TkAssign), generic_argument.type)
            generic_arguments.replace(generic_argument, new_argument)

        # Infer all the generic from arguments, and analyse the new generic-complete type. This type is required to be
        # analysed so that the type-checking of arguments can be done.
        self.class_type.parts[-1].generic_arguments = GenericArgumentGroupAst.from_dict(infer_generics_types(
            Seq(type_symbol.type.generic_parameters.get_req()).map(lambda p: p.identifier).value,
            Seq(self.class_type.parts[-1].generic_arguments.arguments).map(lambda a: (a.identifier, a.type)).dict(),
            Seq(self.arguments.arguments).map(lambda a: (a.identifier, a.value.infer_type(scope_handler, **kwargs))).dict(),
            Seq(type_symbol.type.body.members).map(lambda a: (a.identifier, a.type_declaration)).dict()))
        self.class_type.do_semantic_analysis(scope_handler, **kwargs)

        # Type-check the arguments of the object initializer.
        self.arguments.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # The returning type of the object initializer is the type of the object being constructed.
        return InferredType(convention=ConventionMovAst, type=self._modified_type)


__all__ = ["ObjectInitializerAst"]

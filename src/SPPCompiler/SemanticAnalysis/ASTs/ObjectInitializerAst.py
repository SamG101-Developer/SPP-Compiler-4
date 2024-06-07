from dataclasses import dataclass, field
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from SPPCompiler.SemanticAnalysis.ASTs.ObjectInitializerArgumentGroupAst import ObjectInitializerArgumentGroupAst
from SPPCompiler.SemanticAnalysis.ASTs.TypeAst import TypeAst


@dataclass
class ObjectInitializerAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The ObjectInitializerAst node represents the construction of an object of a class type, with the given arguments.

    Attributes:
        - class_type: The type of the object to be constructed.
        - arguments: The arguments to be given to the object's constructor.

        - _modified_type: The type of the object to be constructed, with generic arguments inferred.
    """

    class_type: TypeAst
    arguments: ObjectInitializerArgumentGroupAst
    _modified_type: Optional[TypeAst] = field(default=None, init=False)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ObjectInitializerAst.
        s = ""
        s += f"{self.class_type.print(printer)}"
        s += f"{self.arguments.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        # The returning type of the object initializer is the type of the object being constructed.
        return InferredType(convention=ConventionMovAst, type=self._modified_type)


__all__ = ["ObjectInitializerAst"]

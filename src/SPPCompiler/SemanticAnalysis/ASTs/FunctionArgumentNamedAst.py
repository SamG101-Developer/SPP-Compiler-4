from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors


@dataclass
class FunctionArgumentNamedAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The GenericArgumentNamedAst node represents a named generic argument being given to a generic parameter of a
    function/class/superimposition block. This looks like `function_call[T=Str](123)`, where `T=Str` is the generic
    argument being given to the function. The argument has an identifier, assignment token, and the type.

    Attributes:
        identifier: The identifier of the argument.
        assignment_token: The token representing the assignment of the argument.
        convention: The convention of the argument.
        value: The value of the argument.
    """

    identifier: "IdentifierAst"
    assignment_token: "TokenAst"
    convention: "ConventionAst"
    value: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionArgumentNamedAst.
        s = ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.assignment_token.print(printer)}"
        s += f"{self.convention.print(printer)}"
        s += f"{self.value.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the value of the argument.
        self.value.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

        # The convention of an argument is either the given convention, or the convention of the value.
        match self.convention, self.value.infer_type(scope_handler, **kwargs).convention:
            case ConventionMovAst(), that_convention: convention = that_convention
            case self_convention, _: convention = type(self_convention)

        return InferredType(convention=convention, type=self.value.infer_type(scope_handler, **kwargs).type)


__all__ = ["FunctionArgumentNamedAst"]

from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class FunctionArgumentNormalAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The FunctionArgumentNormalAst node represents a non-named argument being given to a parameter of a function. This
    looks like `function_call(123)`, where `123` is the argument being given to the function. The argument has a
    convention (no symbol => mov), an optional unpack token if a tuple is being unpacked, and the value of the argument.

    Attributes:
        convention: The convention of the argument.
        unpack_token: The token representing the unpacking of the argument.
        value: The value of the argument.
    """

    convention: "ConventionAst"
    unpack_token: Optional["TokenAst"]
    value: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionArgumentNormalAst.
        s = ""
        s += f"{self.convention.print(printer)}"
        s += f"{self.unpack_token.print(printer)}" if self.unpack_token else ""
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


__all__ = ["FunctionArgumentNormalAst"]

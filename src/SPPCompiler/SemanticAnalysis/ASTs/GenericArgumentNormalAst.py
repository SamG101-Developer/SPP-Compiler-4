from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class GenericArgumentNormalAst(Ast, SemanticAnalyser):
    """
    The GenericArgumentNormalAst node represents a non-named generic argument being given to a generic parameter of a
    function/class/superimposition block. This looks like `function_call[Str](123)`, where `Str` is the generic argument
    being given to the function.

    Attributes:
        type: The type that the generic argument contains.
    """

    type: "TypeAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the GenericArgumentNormalAst.
        s = ""
        s += f"{self.type.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the type of the argument.
        self.type.do_semantic_analysis(scope_handler, **kwargs)

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same type.
        return isinstance(other, GenericArgumentNormalAst) and self.type == other.type


__all__ = ["GenericArgumentNormalAst"]

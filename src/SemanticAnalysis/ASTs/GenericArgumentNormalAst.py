from dataclasses import dataclass

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class GenericArgumentNormalAst(Ast, SemanticAnalysis):
    """
    The GenericArgumentNormalAst node represents a non-named generic argument being given to a generic parameter of a
    function/class/superimposition block. This looks like `function_call[Str](123)`, where `Str` is the generic argument
    being given to the function.

    Attributes:
        - type: The type that the generic argument contains.
    """

    type: "TypeAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the GenericArgumentNormalAst.
        s = ""
        s += f"{self.type.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the type contained by the generic argument.
        self.type.do_semantic_analysis(scope_handler, **kwargs)

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same type.
        return isinstance(other, GenericArgumentNormalAst) and self.type == other.type


__all__ = ["GenericArgumentNormalAst"]

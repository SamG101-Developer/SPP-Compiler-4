from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst


@dataclass
class ObjectInitializerArgumentNormalAst(Ast, SemanticAnalyser):
    """
    The ObjectInitializerArgumentNormalAst node is used to represent a normal argument of an object initializer. This is
    when a variable matches the name of an attribute on the target type.

    Attributes:
        identifier: The identifier of the target field (and variable being moved into it).
    """

    identifier: IdentifierAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ObjectInitializerArgumentNormalAst.
        s = ""
        s += f"{self.identifier.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the identifier of the normal argument.
        self.identifier.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["ObjectInitializerArgumentNormalAst"]

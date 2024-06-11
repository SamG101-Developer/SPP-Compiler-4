from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class ObjectInitializerArgumentNamedAst(Ast, SemanticAnalyser):
    """
    The ObjectInitializerArgumentNamedAst node is used to represent a named argument of an object initializer. This is
    when a field name and value are provided in the object initializer. The identifier can be the keyword "sup" for
    potential superclasses, or "else" for the optional default value.

    Attributes:
        identifier: The identifier of the target field.
    """

    identifier: "IdentifierAst | TokenAst"
    assignment_token: "TokenAst"
    value: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ObjectInitializerArgumentNamedAst.
        s = ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.assignment_token.print(printer)}"
        s += f"{self.value.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the value of the named argument.
        self.value.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["ObjectInitializerArgumentNamedAst"]

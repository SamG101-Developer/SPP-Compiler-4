from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class FunctionParameterVariadicAst(Ast, SemanticAnalyser):
    """
    The FunctionParameterVariadicAst node represents a variadic parameter of a function. This is a parameter that can
    be given any number of arguments when calling the function. The parameter can have a convention, that all the arguments
    must match when calling the function.

    Attributes:
        - variadic_token: The variadic token.
        - identifier: The identifier of the parameter.
        - colon_token: The colon token.
        - convention: The convention of the parameter.
        - type_declaration: The type declaration of the parameter.
    """

    variadic_token: "TokenAst"
    variable: "LocalVariableAst"
    colon_token: "TokenAst"
    convention: "ConventionAst"
    type_declaration: "TypeAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionParameterVariadicAst.
        s = ""
        s += f"{self.variadic_token.print(printer)}"
        s += f"{self.variable.print(printer)}{self.colon_token.print(printer)} "
        s += f"{self.convention.print(printer)}{self.type_declaration.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        ...

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same identifier.
        return isinstance(other, FunctionParameterVariadicAst) and self.variable == other.variable


__all__ = ["FunctionParameterVariadicAst"]

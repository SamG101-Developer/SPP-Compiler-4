from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class FunctionParameterOptionalAst(Ast, SemanticAnalyser):
    """
    The FunctionParameterOptionalAst node represents an optional parameter of a function. This is a parameter that can
    be given an argument when calling the function. The parameter cannot have a convention (ie must be mov/copy). The type
    declaration is still required despite a default value being provided; this is a design decision.

    Attributes:
        - is_mutable: The token that represents the mutability of the parameter.
        - identifier: The identifier of the parameter.
        - colon_token: The colon token.
        - convention: The convention of the parameter.
        - type_declaration: The type declaration of the parameter.
        - assignment_token: The assignment token.
        - default_value: The default value of the parameter.
    """

    variable: "LocalVariableAst"
    colon_token: "TokenAst"
    convention: "ConventionAst"
    type_declaration: "TypeAst"
    assignment_token: "TokenAst"
    default_value: "ExpressionAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionParameterOptionalAst.
        s = ""
        s += f"{self.variable.print(printer)}{self.colon_token.print(printer)} "
        s += f"{self.convention.print(printer)}{self.type_declaration.print(printer)} "
        s += f"{self.assignment_token.print(printer)} {self.default_value.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        ...

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same identifier.
        return isinstance(other, FunctionParameterOptionalAst) and self.variable == other.variable


__all__ = ["FunctionParameterOptionalAst"]

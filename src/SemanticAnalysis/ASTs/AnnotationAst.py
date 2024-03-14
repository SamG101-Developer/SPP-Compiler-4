from __future__ import annotations
from dataclasses import dataclass

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class AnnotationAst(Ast):
    """
    The AnnotationAst node is used to represent an annotation of a class, function, typedef etc. This allows for the
    manipulation of the token stream to alter the code before semantic analysis takes place. This won't be implemented
    until the compiler is self-hosting, because the code needs to be parsed and replaced in the token stream.

    Attributes:
        - at_token: The @ token.
        - identifier: The identifier of the annotation.
        - generic_arguments: The generic arguments of the annotation.
        - arguments: The arguments of the annotation.
    """

    at_token: "TokenAst"
    identifier: "ModuleIdentifierAst"
    generic_arguments: "GenericArgumentGroupAst"
    arguments: "FunctionArgumentGroupAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the AnnotationAst.
        s = ""
        s += f"{self.at_token.print(printer)}"
        s += f"{self.identifier.print(printer)}"
        s += f"{self.generic_arguments.print(printer)}"
        s += f"{self.arguments.print(printer)}"
        return s


__all__ = ["AnnotationAst"]

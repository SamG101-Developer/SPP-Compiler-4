from dataclasses import dataclass

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class LocalVariableAssignmentAst(Ast):
    """
    The LocalVariableAssignmentAst node represents a single local variable with a pre-assigned value. This is only used
    in the pattern matching. For example, in the statement "case point == then Point(x=5, ..) { ... }", "x" is being
    pre-assigned a value (created as a variable without "let").

    Attributes:
        - identifier: The identifier of the variable.
        - assign_token: The assignment token.
        - value: The value being pre-assigned to the variable.
    """

    identifier: "IdentifierAst"
    assign_token: "TokenAst"
    value: "LocalVariableNestedAst"

    def print(self, printer: AstPrinter) -> str:
        # Print the LocalVariableAssignmentAst.
        s = ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.assign_token.print(printer)}"
        s += f"{self.value.print(printer)}"
        return s


__all__ = ["LocalVariableAssignmentAst"]

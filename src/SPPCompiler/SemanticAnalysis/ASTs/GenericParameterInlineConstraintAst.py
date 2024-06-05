from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class GenericParameterInlineConstraintAst(Ast):
    """
    The GenericParameterInlineConstraintAst node is used to represent a list of types that some generic parameter must
    conform to. Each generic parameter can have constraints attached.

    Attributes:
        - colon_token: The colon token.
        - constraints: The constraints of the generic parameter.
    """

    colon_token: "TokenAst"
    constraints: List["TypeAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the GenericParameterInlineConstraintAst.
        s = ""
        s += f"{self.colon_token.print(printer)} "
        s += Seq(self.constraints).print(printer, ", ")
        return s

from dataclasses import dataclass, field
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class LocalVariableSkipArgumentsAst(Ast):
    """
    The LocalVariableSkipArgumentsAst node represents a placeholder variable that is skipping values. This is used
    inside tuple and object destructuring. For example, in the statement "let Point(x, ..) = point", ".." is the skip
    argument, skipping all other fields in the "Point" class. In the tuple destructure, a skip can contain a binding,
    which collects all the remaining values in the tuple.

    Attributes:
        variadic_token: The ".." token.
        binding: The binding of the skip argument. This is only used in tuple destructuring.
    """

    variadic_token: "TokenAst"
    binding: Optional["LocalVariableSingleAst"]
    _num_skipped: int = field(default=0, init=False, repr=False)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LocalVariableSkipArgumentAst.
        s = ""
        s += f"{self.variadic_token.print(printer)}"
        s += f"{self.binding.print(printer)}" if self.binding else ""
        return s


__all__ = ["LocalVariableSkipArgumentsAst"]

from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.NumberLiteralBaseNAst import NumberLiteralBaseNAst


@dataclass
class NumberLiteralBase10Ast(NumberLiteralBaseNAst):
    """
    The LiteralNumberBase10Ast node is used to represent a number literal in base 10 (decimal). This is a literal number
    with no prefix. It can have an optional sign.

    Attributes:
        sign: The optional sign of the number.
    """

    sign: Optional["TokenAst"]

    def print(self, printer) -> str:
        # Print the NumberLiteralBase10Ast.
        s = ""
        s += f"{self.sign.print(printer)}" if self.sign else ""
        s += f"{super().print(printer)}"
        return s


__all__ = ["NumberLiteralBase10Ast"]

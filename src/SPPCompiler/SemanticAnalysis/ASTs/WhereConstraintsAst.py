from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst
from SPPCompiler.SemanticAnalysis.ASTs.TypeAst import TypeAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class WhereConstraintsAst(Ast):
    """
    The WhereConstrainsAst node is used to list constraints to a number of types. Multiple WhereConstraintsAst nodes
    can be provided in the parent WhereConstraintsGroupAst node.

    Attributes:
        types_to_constrain: The types to constrain.
        colon_token: The colon token.
        constraints: The constraints of the types.
    """

    types_to_constrain: List[TypeAst]
    colon_token: TokenAst
    constraints: List[TypeAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the WhereConstraintsAst.
        s = ""
        s += f"{Seq(self.types_to_constrain).print(printer, ", ")}"
        s += f"{self.colon_token.print(printer)} "
        s += f"{Seq(self.constraints).print(printer, ", ")}"
        return s

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same types to constrain and constraints.
        return isinstance(other, WhereConstraintsAst) and self.types_to_constrain == other.types_to_constrain and self.constraints == other.constraints


__all__ = ["WhereConstraintsAst"]

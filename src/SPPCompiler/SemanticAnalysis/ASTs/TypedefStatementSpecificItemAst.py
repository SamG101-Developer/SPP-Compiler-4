from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class TypedefStatementSpecificItemAst(Ast):
    """
    The TypedefStatementSpecificItemAst is used to either reduce the namespace of a type, or to alias the type to a new
    type. For example, "use namespace.OldType" allows the use of "OldType", and "use namespace.OldType as NewType"
    allows the use of "NewType" as an alias for "namespace.OldType".

    Attributes:
        old_type: The old type.
        alias: The alias of the old type.
    """

    old_type: "TypeAst"
    alias: Optional["TypedefStatementSpecificItemAliasAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypedefStatementSpecificItemAst.
        s = ""
        s += f"{self.old_type.print(printer)}"
        s += f"{self.alias.print(printer)}" if self.alias else ""
        return s


__all__ = ["TypedefStatementSpecificItemAst"]

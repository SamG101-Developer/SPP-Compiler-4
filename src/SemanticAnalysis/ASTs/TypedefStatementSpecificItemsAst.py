from dataclasses import dataclass
from typing import List

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypedefStatementSpecificItemAst import TypedefStatementSpecificItemAst

from src.Utils.Sequence import Seq


@dataclass
class TypedefStatementSpecificItemsAst(Ast):
    """
    The TypedefStatementSpecificItemsAst is used to either reduce the namespace of multiple types, or to alias the types
    to new types. For example, "use namespace.(OldType1, OldType2)" allows the use of "OldType1" and "OldType2", and
    "use namespace.(OldType1 as NewType1, OldType2 as OldType2)" allows the use of "NewType1" and "NewType2" as aliases
    for "namespace.OldType1" and "namespace.OldType2" respectively.

    Attributes:
        - old_type: The old type.
        - alias: The alias of the old type.
    """

    paren_l_token: TokenAst
    aliases: List[TypedefStatementSpecificItemAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TypedefStatementSpecificItemsAst.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.aliases).print(printer, ", ")}"
        s += f"{self.paren_r_token.print(printer)}"
        return s


__all__ = ["TypedefStatementSpecificItemsAst"]

from dataclasses import dataclass

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.TypeAst import TypeAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class TypedefStatementSpecificItemAliasAst(Ast):
    """
    The TypedefStatementSpecificItemAliasAst is used to represent the alias of an old type in a typedef statement. The
    alias requires the "as" keyword, and the "new_type".

    Attributes:
        - as_keyword: The "as" keyword token.
        - new_type: The new type.
    """

    as_keyword: TokenAst
    new_type: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        s = ""
        s += f"{self.as_keyword.print(printer)} "
        s += f"{self.new_type.print(printer)}"
        return s


__all__ = ["TypedefStatementSpecificItemAliasAst"]

from dataclasses import dataclass
from typing import List

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst

from src.Utils.Sequence import Seq


# TODO: semantic analysis to ensure the module identifier matches the directory structure from "src".
@dataclass
class ModuleIdentifierAst(Ast):
    """
    The ModuleIdentifierAst node is used to represent the module identifier of a ".spp" file. This is a sequence of
    IdentifierAsts, separated by dots.
    """

    parts: List[IdentifierAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ModuleIdentifierAst.
        s = ""
        s += f"{Seq(self.parts).print(printer, '.')}"
        return s


__all__ = ["ModuleIdentifierAst"]

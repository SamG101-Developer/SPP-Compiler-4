from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from SPPCompiler.Utils.Sequence import Seq


# TODO: semantic analysis to ensure the module identifier matches the directory structure from "src".
@dataclass
class ModuleIdentifierAst(Ast):
    """
    The ModuleIdentifierAst node is used to represent the module identifier of a ".spp" file. This is a sequence of
    IdentifierAsts, separated by dots.

    Attributes:
        parts: The parts of the module identifier.
    """

    parts: List[IdentifierAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ModuleIdentifierAst.
        s = ""
        s += f"{Seq(self.parts).print(printer, '.')}"
        return s


__all__ = ["ModuleIdentifierAst"]

from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ModuleIdentifierAst(Ast, SemanticAnalyser):
    """
    The ModuleIdentifierAst node is used to represent the module identifier of a ".spp" file. This is a sequence of
    IdentifierAsts, separated by dots.

    Attributes:
        parts: The parts of the module identifier.
    """

    parts: List["IdentifierAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ModuleIdentifierAst.
        s = ""
        s += f"{Seq(self.parts).print(printer, '::')}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Perform semantic analysis on the ModuleIdentifierAst.
        module_namespace = kwargs["file-name"]
        module_namespace = module_namespace[module_namespace.index("src") + 4 : -4].replace("\\", "::")
        if str(module_namespace) != str(self):
            raise SemanticErrors.MODULE_NS(self, module_namespace)


__all__ = ["ModuleIdentifierAst"]

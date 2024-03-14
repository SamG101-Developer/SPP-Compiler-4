from dataclasses import dataclass
from typing import List

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.AnnotationAst import AnnotationAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.ModuleIdentifierAst import ModuleIdentifierAst
from src.SemanticAnalysis.ASTs.ModuleImplementationAst import ModuleImplementationAst

from src.Utils.Sequence import Seq


@dataclass
class ModulePrototypeAst(Ast, SemanticAnalysis):
    """
    The ModulePrototypeAst node represents a module definition, containing the module identifier and implementation.
    Annotations can also be attached.

    Attributes:
        - annotations: The annotations attached to the module.
        - module_keyword: The "mod" keyword token.
        - identifier: The module identifier.
        - body: The module implementation.
    """

    annotations: List[AnnotationAst]
    module_keyword: TokenAst
    identifier: ModuleIdentifierAst
    body: ModuleImplementationAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ModulePrototypeAst.
        s = ""
        s += f"{Seq(self.annotations).print(printer, '\n')}\n"
        s += f"{self.module_keyword.print(printer)}"
        s += f"{self.identifier.print(printer)}\n"
        s += f"{self.body.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        Seq(self.annotations).for_each(lambda x: x.do_semantic_analysis(scope_handler, **kwargs))
        # self.identifier.do_semantic_analysis(s)
        self.body.do_semantic_analysis(scope_handler, **kwargs)


__all__ = ["ModulePrototypeAst"]

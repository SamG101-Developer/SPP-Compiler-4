from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.AnnotationAst import AnnotationAst
from SPPCompiler.SemanticAnalysis.ASTs.SupPrototypeAst import SupPrototypeAst
from SPPCompiler.SemanticAnalysis.ASTs.TypedefStatementAst import TypedefStatementAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class SupTypedefAst(TypedefStatementAst):
    """
    The SupTypedefAst node is a TypedefAst node for a SupPrototypeAst node (normal or inheritance). The only addition it
    has compared to a normal TypedefAst node is the annotations that can be attached to it.

    Attributes:
        - annotations: The annotations attached to the typedef.
    """

    annotations: List[AnnotationAst]

    def __post_init__(self):
        raise NotImplementedError("SupTypedefAst is not implemented yet.")

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the SupTypedefAst.
        s = ""
        s += f"{Seq(self.annotations).print(printer, "\n")}\n"
        s += super().print(printer)
        return s

    def pre_process(self, context: SupPrototypeAst) -> None:
        ...


__all__ = ["SupTypedefAst"]

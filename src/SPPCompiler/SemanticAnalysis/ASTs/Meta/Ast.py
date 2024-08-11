from __future__ import annotations
from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class Ast:
    pos: int
    
    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        ...
    
    def __eq__(self, other):
        return isinstance(other, Ast)

    def __str__(self):
        printer = AstPrinter()
        return self.print(printer)


class Default:
    @staticmethod
    def default() -> Default:
        ...


__all__ = ["Ast", "Default"]

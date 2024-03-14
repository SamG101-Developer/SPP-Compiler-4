from __future__ import annotations

from abc import ABC, abstractmethod
from dataclasses import dataclass

from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class Ast(ABC):
    pos: int
    
    @ast_printer_method
    @abstractmethod
    def print(self, printer: AstPrinter) -> str:
        ...
    
    def __eq__(self, other):
        return isinstance(other, Ast)

    def __str__(self):
        printer = AstPrinter()
        return self.print(printer)


class Default(ABC):
    @staticmethod
    @abstractmethod
    def default():
        ...


__all__ = ["Ast", "Default"]

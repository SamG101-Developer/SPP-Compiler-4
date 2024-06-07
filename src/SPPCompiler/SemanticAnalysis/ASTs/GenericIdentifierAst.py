from __future__ import annotations
from dataclasses import dataclass
from typing import Optional

from SPPCompiler.LexicalAnalysis.Tokens import TokenType

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.GenericArgumentGroupAst import GenericArgumentGroupAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst


@dataclass
class GenericIdentifierAst(Ast):
    """
    The GenericIdentifierAst node represents a generic identifier. This is an identifier that can have generic arguments
    applied to it, typically making up part of a type identifier. For example, `Vec[T]` is a type containing a generic
    identifier `Vec` with a single generic argument `T`. It will start with an uppercase letter.

    Attributes:
        value: The value of the generic identifier.
        generic_arguments: The generic arguments of the generic identifier.
    """

    value: str
    generic_arguments: Optional[GenericArgumentGroupAst]

    def __post_init__(self):
        self.generic_arguments = self.generic_arguments or GenericArgumentGroupAst(-1, TokenAst.dummy(TokenType.TkBrackL), [], TokenAst.dummy(TokenType.TkBrackR))

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the GenericIdentifierAst.
        s = ""
        s += f"{self.value}"
        s += f"{self.generic_arguments.print(printer)}" if self.generic_arguments else ""
        return s

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same value and generic arguments.
        return isinstance(other, GenericIdentifierAst) and self.value == other.value and self.generic_arguments == other.generic_arguments


__all__ = ["GenericIdentifierAst"]

from __future__ import annotations
from dataclasses import dataclass, field
from typing import Optional

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst
from src.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst


@dataclass
class GenericParameterRequiredAst(Ast, SemanticAnalysis):
    """
    The GenericParameterRequiredAst node represents a required generic parameter of a function/class/superimposition
    prototype. This is a generic parameter that must always be given a generic argument when calling the function or
    instantiating the object (if it isn't inferrable).

    Attributes:
        - raw_identifier: The raw identifier of the generic parameter.
        - inline_constraints: The inline constraints of the generic parameter.
        - identifier: The converted identifier of the generic parameter.
    """

    raw_identifier: "IdentifierAst"
    inline_constraints: Optional["GenericParameterInlineConstraintAst"]
    identifier: "TypeAst" = field(default=None, init=False)

    def __post_init__(self):
        # Convert the raw identifier into a TypeSingleAst.
        self.identifier = TypeSingleAst(self.raw_identifier.pos, [GenericIdentifierAst(self.raw_identifier.pos, self.raw_identifier.value, None)])

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the GenericParameterRequiredAst.
        s = ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.inline_constraints.print(printer)}" if self.inline_constraints else ""
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Because this is a type-oriented AST, the generating stage of function/class/superimposition blocks will have
        # registered these generic types into the scope.
        ...


__all__ = ["GenericParameterRequiredAst"]

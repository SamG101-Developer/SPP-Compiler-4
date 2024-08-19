import hashlib
from dataclasses import dataclass, field
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class GenericParameterOptionalAst(Ast, SemanticAnalyser):
    """
    The GenericParameterOptionalAst node represents an optional generic parameter of a function/class/superimposition
    prototype. This is a parameter that can be given a generic argument when calling the function or instantiating the
    object.

    Attributes:
        raw_identifier: The raw identifier of the generic parameter.
        inline_constraints: The inline constraints of the generic parameter.
        assignment_token: The assignment token.
        default_value: The default value of the generic parameter.
        identifier: The converted identifier of the generic parameter.
    """

    raw_identifier: "IdentifierAst"
    inline_constraints: Optional["GenericParameterInlineConstraintAst"]
    assignment_token: "TokenAst"
    default_value: "TypeAst"
    identifier: "TypeAst" = field(default=None, init=False)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        self.identifier = TypeAst(self.raw_identifier.pos, [], [self.raw_identifier.to_generic_identifier()])

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the GenericParameterOptionalAst.
        s = ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.inline_constraints.print(printer)} " if self.inline_constraints else " "
        s += f"{self.assignment_token.print(printer)} "
        s += f"{self.default_value.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    def __hash__(self):
        return int.from_bytes(hashlib.md5(str(self).encode()).digest(), byteorder="big")


__all__ = ["GenericParameterOptionalAst"]

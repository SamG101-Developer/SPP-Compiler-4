import hashlib
from dataclasses import dataclass, field
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class GenericParameterVariadicAst(Ast, SemanticAnalyser):
    """
    The GenericParameterVariadicAst node represents a variadic generic parameter of a function/class/superimposition
    prototype. This is a generic parameter that can be given any number of generic arguments when calling the function
    or instantiating the object.

    Attributes:
        variadic_token: The variadic token.
        raw_identifier: The raw identifier of the generic parameter.
        inline_constraints: The inline constraints of the generic parameter.
        identifier: The converted identifier of the generic parameter.
    """

    variadic_token: "TokenAst"
    raw_identifier: "IdentifierAst"
    inline_constraints: Optional["GenericParameterInlineConstraintAst"]
    identifier: "TypeAst" = field(default=None, init=False)

    def __post_init__(self):
        # Convert the raw identifier into a TypeSingleAst.
        from SPPCompiler.SemanticAnalysis.ASTs import TypeAst
        self.identifier = TypeAst(self.raw_identifier.pos, [], [self.raw_identifier.to_generic_identifier()])

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the GenericParameterVariadicAst.
        s = ""
        s += f"{self.variadic_token.print(printer)}"
        s += f"{self.identifier.print(printer)}"
        s += f"{self.inline_constraints.print(printer)}" if self.inline_constraints else ""
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    def __hash__(self):
        return int.from_bytes(hashlib.md5(str(self).encode()).digest(), byteorder="big")


__all__ = ["GenericParameterVariadicAst"]

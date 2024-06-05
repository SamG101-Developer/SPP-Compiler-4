from __future__ import annotations
from dataclasses import dataclass, field

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class GenericArgumentNamedAst(Ast, SemanticAnalyser):
    """
    The FunctionArgumentNamedAst node represents a named argument being given to a parameter of a function. This looks
    like `function_call(x=123)`, where `x=123` is the argument being given to the function. The argument has an
    identifier, convention (no symbol => mov), and the value of the argument.

    Attributes:
        - identifier: The identifier of the argument.
        - assignment_token: The token that represents the assignment of the argument.
        - convention: The convention of the argument.
        - value: The value of the argument.
    """

    raw_identifier: "IdentifierAst"
    assignment_token: "TokenAst"
    type: "TypeAst"
    identifier: "TypeAst" = field(default=None, init=False)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs.TypeSingleAst import TypeSingleAst
        from SPPCompiler.SemanticAnalysis.ASTs.GenericIdentifierAst import GenericIdentifierAst
        self.identifier = TypeSingleAst(self.raw_identifier.pos, [GenericIdentifierAst(self.raw_identifier.pos, self.raw_identifier.value, None)])

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionArgumentNamedAst.
        s = ""
        s += f"{self.raw_identifier.print(printer)}"
        s += f"{self.assignment_token.print(printer)}"
        s += f"{self.type.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same identifier and type.
        return isinstance(other, GenericArgumentNamedAst) and self.identifier == other.identifier and self.type == other.type

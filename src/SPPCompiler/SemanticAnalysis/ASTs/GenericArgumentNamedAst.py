from __future__ import annotations
from dataclasses import dataclass, field

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol


@dataclass
class GenericArgumentNamedAst(Ast, SemanticAnalyser):
    """
    The GenericArgumentNamedAst node represents a named generic argument being given to a generic parameter of a
    function/class/superimposition block. This looks like `function_call[T=Str](x=123)`, where `T=123` is the argument
    being given to the function.

    Attributes:
        raw_identifier: The identifier of the argument.
        assignment_token: The token that represents the assignment of the argument.
        type: The type of the argument.
        identifier: The identifier of the argument.
    """

    raw_identifier: "IdentifierAst"
    assignment_token: "TokenAst"
    type: "TypeAst"
    identifier: "TypeAst" = field(default=None, init=False)

    def __post_init__(self):
        from SPPCompiler.SemanticAnalysis.ASTs.TypeAst import TypeAst
        self.identifier = TypeAst(self.raw_identifier.pos, [], [self.raw_identifier.to_generic_identifier()])

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionArgumentNamedAst.
        s = ""
        s += f"{self.identifier.print(printer)}"
        s += f"{self.assignment_token.print(printer)}"
        s += f"{self.type.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the type of the argument.
        self.type.do_semantic_analysis(scope_handler, **kwargs)

    @staticmethod
    def from_symbol(symbol: TypeSymbol) -> GenericArgumentNamedAst:
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst

        return GenericArgumentNamedAst(
            pos=-1,
            raw_identifier=symbol.name.to_identifier(),
            assignment_token=TokenAst.dummy(TokenType.TkAssign),
            type=symbol.associated_scope.associated_type_symbol.fq_type)

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same identifier and type.
        return isinstance(other, GenericArgumentNamedAst) and self.identifier == other.identifier and self.type == other.type


__all__ = ["GenericArgumentNamedAst"]

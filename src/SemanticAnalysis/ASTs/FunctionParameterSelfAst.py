from dataclasses import dataclass, field
from typing import Optional

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Types.CommonTypes import CommonTypes
from src.SemanticAnalysis.Symbols.Symbols import VariableSymbol, MemoryStatus

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from src.SemanticAnalysis.ASTs.ConventionRefAst import ConventionRefAst
from src.SemanticAnalysis.ASTs.ConventionMutAst import ConventionMutAst
from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst


@dataclass
class FunctionParameterSelfAst(Ast, SemanticAnalysis):
    """
    The FunctionParameterSelfAst node represents the "self" parameter of a function. This is a special parameter that
    represents the instance of the type that the function is being called on. The "self" parameter is always the first
    parameter of a function. It can have conventions too (no convention means that the object is consumed in the call).

    Attributes:
        - is_mutable: The token that represents the mutability of the "self" parameter.
        - convention: The convention of the "self" parameter.
        - identifier: The identifier of the "self" parameter.
        - type_declaration: The type declaration of the "self" parameter.
    """

    is_mutable: Optional[TokenAst]
    convention: ConventionAst
    identifier: IdentifierAst
    type_declaration: TypeAst = field(default=None, init=False)

    def __post_init__(self):
        # TODO: remove the "self" identifier from parser and generate it in here (cleaner parser)
        # Set the "self" symbol's type to the "Self" type.
        self.type_declaration = CommonTypes.self()

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionParameterSelfAst.
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.convention.print(printer)} {self.identifier.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Add the "self" symbol to the current scope. The memory status depends on the calling-convention used on the
        # "self" parameter, ie {"&" => immutable borrow, "&mut" => mutable borrow}.
        symbol = VariableSymbol(self.identifier, CommonTypes.self(), is_mutable=self.is_mutable is not None, memory_info=MemoryStatus(
            ast_initialized=self.identifier,
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self.convention))
        scope_handler.current_scope.add_symbol(symbol)

    def __eq__(self, other):
        # Check both ASTs are the same type.
        return isinstance(other, FunctionParameterSelfAst)


__all__ = ["FunctionParameterSelfAst"]

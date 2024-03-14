from dataclasses import dataclass

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Symbols.Symbols import VariableSymbol, MemoryStatus

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class LetStatementUninitializedAst(Ast, SemanticAnalysis):
    """
    The LetStatementUninitializedAst node represents a "let" statement where a value is not provided to the variable.
    This means that the variable cannot be used until it has been assigned a value. Uninitialized variables must be
    given a type declaration.

    Attributes:
        - let_keyword: The "let" keyword token.
        - assign_to: The variable being assigned to.
        - colon_token: The colon token.
        - type_declaration: The variable's type.
    """

    let_keyword: "TokenAst"
    assign_to: "LocalVariableAst"
    colon_token: "TokenAst"
    type_declaration: "TypeAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LetStatementUninitializedAst.
        s = ""
        s += f"{self.let_keyword.print(printer)}"
        s += f"{self.assign_to.print(printer)}"
        s += f"{self.colon_token.print(printer)} "
        s += f"{self.type_declaration.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Create a variable symbol for the variable being assigned to, marked as "consumed".
        sym = VariableSymbol(
            name=self.assign_to.identifier,
            type=self.type_declaration,
            is_mutable=self.assign_to.is_mutable is not None,
            memory_info=MemoryStatus(ast_consumed=self))

        # Add the symbol to the current scope.
        scope_handler.current_scope.add_symbol(sym)


__all__ = ["LetStatementUninitializedAst"]

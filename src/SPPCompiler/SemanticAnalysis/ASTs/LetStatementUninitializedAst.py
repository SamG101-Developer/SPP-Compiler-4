from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol, MemoryStatus

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class LetStatementUninitializedAst(Ast, SemanticAnalyser):
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
        ...


__all__ = ["LetStatementUninitializedAst"]

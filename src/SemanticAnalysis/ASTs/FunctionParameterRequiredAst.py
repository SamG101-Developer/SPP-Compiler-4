from dataclasses import dataclass
from typing import Optional

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.Symbols import VariableSymbol, MemoryStatus

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from src.SemanticAnalysis.ASTs.ConventionRefAst import ConventionRefAst
from src.SemanticAnalysis.ASTs.ConventionMutAst import ConventionMutAst
from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.TypeAst import TypeAst


@dataclass
class FunctionParameterRequiredAst(Ast, SemanticAnalyser):
    """
    The FunctionParameterRequiredAst node represents a required parameter of a function. This is a parameter that must
    always be given an argument when calling the function. The parameter can have a convention, that the argument must
    match when calling the function.

    Attributes:
        - is_mutable: The token that represents the mutability of the parameter.
        - identifier: The identifier of the parameter.
        - colon_token: The colon token.
        - convention: The convention of the parameter.
        - type_declaration: The type declaration of the parameter.
    """

    is_mutable: Optional[TokenAst]
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: ConventionAst
    type_declaration: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionParameterRequiredAst.
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.identifier.print(printer)}{self.colon_token.print(printer)} {self.convention.print(printer)}{self.type_declaration.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Analyse the parameter type.
        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)

        # Add the symbol for the parameter. The memory status depends on the calling-convention used.
        symbol = VariableSymbol(self.identifier, self.type_declaration, is_mutable=self.is_mutable is not None, memory_info=MemoryStatus(
            ast_initialized=self.identifier,
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self.convention))
        scope_handler.current_scope.add_symbol(symbol)

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same identifier.
        return isinstance(other, FunctionParameterRequiredAst) and self.identifier == other.identifier


__all__ = ["FunctionParameterRequiredAst"]

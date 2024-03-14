from dataclasses import dataclass
from typing import Optional

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
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
class FunctionParameterVariadicAst(Ast, SemanticAnalysis):
    """
    The FunctionParameterVariadicAst node represents a variadic parameter of a function. This is a parameter that can
    be given any number of arguments when calling the function. The parameter can have a convention, that all the arguments
    must match when calling the function.

    Attributes:
        - is_mutable: The token that represents the mutability of the parameter.
        - variadic_token: The variadic token.
        - identifier: The identifier of the parameter.
        - colon_token: The colon token.
        - convention: The convention of the parameter.
        - type_declaration: The type declaration of the parameter.
    """

    is_mutable: Optional[TokenAst]
    variadic_token: TokenAst
    identifier: IdentifierAst
    colon_token: TokenAst
    convention: ConventionAst
    type_declaration: TypeAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionParameterVariadicAst.
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.variadic_token.print(printer)}"
        s += f"{self.identifier.print(printer)}"
        s += f"{self.colon_token.print(printer)} "
        s += f"{self.convention.print(printer)}"
        s += f"{self.type_declaration.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # TODO : type declaration for variadics will need to be checked later: tuple?
        # TODO

        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)
        symbol = VariableSymbol(self.identifier, self.type_declaration, is_mutable=self.is_mutable is not None, memory_info=MemoryStatus(
            ast_initialized=self.identifier,
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self.convention))
        scope_handler.current_scope.add_symbol(symbol)

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same identifier.
        return isinstance(other, FunctionParameterVariadicAst) and self.identifier == other.identifier


__all__ = ["FunctionParameterVariadicAst"]

from dataclasses import dataclass, field
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol, MemoryStatus
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class FunctionParameterSelfAst(Ast, SemanticAnalyser):
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

    is_mutable: Optional["TokenAst"]
    convention: "ConventionAst"
    identifier: "IdentifierAst"
    type_declaration: "TypeAst" = field(default=None, init=False)

    def __post_init__(self):
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
        from SPPCompiler.LexicalAnalysis.Tokens import TokenType
        from SPPCompiler.SemanticAnalysis.ASTs import (
            LetStatementUninitializedAst, TokenAst, ConventionRefAst, ConventionMutAst, LocalVariableSingleAst)

        # Convert the parameter to a "let" statement.
        let_statement = LetStatementUninitializedAst(
            pos=self.pos,
            let_keyword=TokenAst.dummy(TokenType.KwLet),
            assign_to=LocalVariableSingleAst(self.pos, self.is_mutable, None, self.identifier),
            colon_token=TokenAst.dummy(TokenType.TkColon),
            type_declaration=self.type_declaration)
        let_statement.do_semantic_analysis(scope_handler, **kwargs)

        # Set the symbol's memory status depending on the convention.
        symbol = scope_handler.current_scope.get_symbol(self.identifier)
        symbol.memory_info = MemoryStatus(
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self.convention,
            ast_initialized=self)

    def __eq__(self, other):
        # Check both ASTs are the same type.
        return isinstance(other, FunctionParameterSelfAst)


__all__ = ["FunctionParameterSelfAst"]

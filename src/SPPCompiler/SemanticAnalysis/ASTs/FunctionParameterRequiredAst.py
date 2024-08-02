from dataclasses import dataclass

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Symbols import MemoryStatus


@dataclass
class FunctionParameterRequiredAst(Ast, SemanticAnalyser):
    """
    The FunctionParameterRequiredAst node represents a required parameter of a function. This is a parameter that must
    always be given an argument when calling the function. The parameter can have a convention, that the argument must
    match when calling the function.

    Attributes:
        variable: The variable of the parameter.
        colon_token: The colon token.
        convention: The convention of the parameter.
        type_declaration: The type declaration of the parameter.
    """

    variable: "LocalVariableAst"
    colon_token: "TokenAst"
    convention: "ConventionAst"
    type_declaration: "TypeAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the FunctionParameterRequiredAst.
        s = ""
        s += f"{self.variable.print(printer)}{self.colon_token.print(printer)} "
        s += f"{self.convention.print(printer)}{self.type_declaration.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import (
            ConventionRefAst, ConventionMutAst, TokenAst, LetStatementUninitializedAst)

        self.type_declaration.do_semantic_analysis(scope_handler, **kwargs)

        # Convert the parameter to a "let" statement.
        let_statement = LetStatementUninitializedAst(
            pos=self.pos,
            let_keyword=TokenAst.dummy(TokenType.KwLet),
            assign_to=self.variable,
            colon_token=self.colon_token,
            type_declaration=self.type_declaration)
        let_statement.do_semantic_analysis(scope_handler, **kwargs)

        # Set the symbol's memory status depending on the convention.
        symbol = scope_handler.current_scope.get_symbol(self.variable.identifier)
        symbol.memory_info = MemoryStatus(
            is_borrow_ref=isinstance(self.convention, ConventionRefAst),
            is_borrow_mut=isinstance(self.convention, ConventionMutAst),
            ast_borrow=self.convention,
            ast_initialized=self)

    def identifier_for_param(self) -> "IdentifierAst":
        from SPPCompiler.SemanticAnalysis.ASTs import LocalVariableSingleIdentifierAst, IdentifierAst
        match self.variable:
            case LocalVariableSingleIdentifierAst(): return self.variable.identifier
            case _: return IdentifierAst(self.pos, "UNMATCHABLE")

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same identifier.
        return isinstance(other, FunctionParameterRequiredAst) and self.variable == other.variable


__all__ = ["FunctionParameterRequiredAst"]

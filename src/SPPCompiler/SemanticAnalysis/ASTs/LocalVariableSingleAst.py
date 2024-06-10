from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol, MemoryStatus


@dataclass
class LocalVariableSingleAst(Ast, SemanticAnalyser):
    """
    The LocalVariableSingleAst node represents a single local variable. This is the most basic form of a local variable,
    and is seen mostly in the "let" statement. For example, in the statement "let mut x = 5", "mut x" is the single
    local variable.

    Attributes:
        is_mutable: The token representing the mutability of the variable.
        unpack_token: The optional unpacking token.
        identifier: The identifier of the variable.
    """

    is_mutable: Optional["TokenAst"]
    unpack_token: Optional["TokenAst"]
    identifier: "IdentifierAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LocalVariableSingleAst.
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.unpack_token.print(printer)}" if self.unpack_token else ""
        s += f"{self.identifier.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        kwargs |= {"assignment": True}

        # Get the value of the local variable, and semantically analyse it. This must happen before type-inference, as
        # inferring an invalid expression could cause an error.
        value = kwargs["value"]
        value.do_semantic_analysis(scope_handler, **kwargs)

        # Create a variable symbol for the local variable and add it to the current scope. Set the initialization AST to
        # the "let" statement AST that contains this local variable.
        print("VV", value.infer_type(scope_handler, **kwargs), "DONE")
        symbol = VariableSymbol(
            name=self.identifier,
            type=value.infer_type(scope_handler, **kwargs).type_symbol.fq_type,
            is_mutable=self.is_mutable is not None,
            memory_info=MemoryStatus(ast_initialized=self))
        scope_handler.current_scope.add_symbol(symbol)


__all__ = ["LocalVariableSingleAst"]

from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Symbols import VariableSymbol, MemoryStatus


@dataclass
class LocalVariableSingleIdentifierAst(Ast, SemanticAnalyser):
    """
    The LocalVariableSingleAst node represents a single local variable. This is the most basic form of a local variable,
    and is seen mostly in the "let" statement. For example, in the statement "let mut x = 5", "mut x" is the single
    local variable.

    Attributes:
        is_mutable: The token representing the mutability of the variable.
        identifier: The identifier of the variable.
    """

    is_mutable: Optional["TokenAst"]
    identifier: "IdentifierAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the LocalVariableSingleAst.
        s = ""
        s += f"{self.is_mutable.print(printer)}" if self.is_mutable else ""
        s += f"{self.identifier.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Get the value of the local variable, and semantically analyse it. This must happen before type-inference, as
        # inferring an invalid expression could cause an error.
        value = kwargs["value"]
        value.do_semantic_analysis(scope_handler, **(kwargs | {"assignment": self.identifier}))

        # Create a variable symbol for the local variable and add it to the current scope. Set the initialization AST to
        # the "let" statement AST that contains this local variable.
        symbol = VariableSymbol(
            name=self.identifier,
            type=value.infer_type(scope_handler, **kwargs).type,
            is_mutable=self.is_mutable is not None,
            memory_info=MemoryStatus(ast_initialized=self.identifier))
        scope_handler.current_scope.add_symbol(symbol)


__all__ = ["LocalVariableSingleIdentifierAst"]

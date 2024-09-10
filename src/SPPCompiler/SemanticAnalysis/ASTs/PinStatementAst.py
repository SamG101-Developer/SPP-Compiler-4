from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import AstPrinter, ast_printer_method
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class PinStatementAst(Ast, SemanticAnalyser):
    """
    The PinStatementAst node represents the "pinning" of a number of variables or attributes. Pinning them ensures they
    are never moved until they are unpinned. This allows async functions and coroutines to use borrows safely (adhering
    to the memory model).

    Pinning "a.x" doesn't pin the rest of "a", but pinning "a" pins all of its attributes, including "a.x".

    Attributes:
        pin_keyword: The "pin" keyword.
        expressions: The expressions to pin.
    """

    pin_keyword: "TokenAst"
    expressions: List["ExpressionAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the PinStatementAst.
        s = ""
        s += f"{self.pin_keyword.print(printer)} "
        s += f"{Seq(self.expressions).print(printer, ", ")};"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse each expression.
        expressions = Seq(self.expressions)
        expressions.for_each(lambda e: e.do_semantic_analysis(scope_handler, **kwargs))

        # Ensure each expression is a variable or attribute: has a symbol.
        symbols = expressions.map(scope_handler.current_scope.get_outermost_variable_symbol)
        if symbols.filter_out_none().length < expressions.length:
            raise SemanticErrors.INVALID_PIN_TARGET(self, expressions[symbols.index(None)])

        # Prevent overlapping pins from being created, enforcing code uniformity.
        symbols.remove_none()
        for pin_target in expressions:
            symbol = scope_handler.current_scope.get_outermost_variable_symbol(pin_target)

            for existing_pin in symbol.memory_info.ast_pins:
                if str(pin_target).startswith(str(existing_pin)) or str(existing_pin).startswith(str(pin_target)):
                    raise SemanticErrors.PIN_OVERLAP_CONFLICT(existing_pin, pin_target)
            symbol.memory_info.ast_pins.append(pin_target)


__all__ = ["PinStatementAst"]

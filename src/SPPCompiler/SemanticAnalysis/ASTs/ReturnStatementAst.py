from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import ensure_memory_integrity
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes


@dataclass
class ReturnStatementAst(Ast, SemanticAnalyser):
    """
    The ReturnStatementAst node represents a return statement. This is a statement that returns a value from a function.
    It has a return keyword, and an optional expression to return.

    Attributes:
        return_keyword: The "ret" keyword.
        expression: The expression to return.
    """

    return_keyword: "TokenAst"
    expression: Optional["ExpressionAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ReturnStatementAst.
        s = ""
        s += f"{self.return_keyword.print(printer)}"
        s += f"{self.expression.print(printer)}" if self.expression else ""
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # Check the value being turned is an owned type.
        if "is-coroutine" in kwargs:
            raise SemanticErrors.RETURN_OUTSIDE_SUBROUTINE(self, kwargs["is-coroutine"])
        if self.expression:
            ensure_memory_integrity(self, self.expression, self.return_keyword, scope_handler)

        # Check the return type matches the enclosing function's return type.
        target_return_type = InferredType(convention=ConventionMovAst, type=kwargs["target-return-type"])
        actual_return_type = self.expression.infer_type(scope_handler, **kwargs) if self.expression else InferredType(convention=ConventionMovAst, type=CommonTypes.void())
        if not actual_return_type.symbolic_eq(target_return_type, scope_handler.current_scope):
            symbol = scope_handler.current_scope.get_outermost_variable_symbol(self.expression)
            raise SemanticErrors.TYPE_MISMATCH_2(None, self.expression or self.return_keyword, target_return_type, actual_return_type, scope_handler)

        # Check there are no synbols pinned; exiting the function would destroy them.
        symbols = Seq(scope_handler.current_scope.all_symbols()).filter_to_type(VariableSymbol)
        for symbol in symbols:
            if not isinstance(symbol.memory_info.ast_initialized, GlobalConstantAst):
                if symbol.memory_info.ast_pins:
                    raise SemanticErrors.MOVING_PINNED_VALUE(self, symbol.memory_info.ast_pins[0])


__all__ = ["ReturnStatementAst"]

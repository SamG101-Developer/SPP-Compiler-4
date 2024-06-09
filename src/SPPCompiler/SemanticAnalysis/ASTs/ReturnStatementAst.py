from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class ReturnStatementAst(Ast, SemanticAnalyser):
    """
    The ReturnStatementAst node represents a return statement. This is a statement that returns a value from a function.
    It has a return keyword and an optional expression to return.

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
        # Check the value being turned is an owned type.
        if self.expression:
            self.expression.do_semantic_analysis(scope_handler, **kwargs)
            symbol = scope_handler.current_scope.get_outermost_variable_symbol(self.expression)
            if symbol and symbol.memory_info.is_borrow:
                raise SemanticErrors.MOVING_FROM_BORROWED_CONTEXT(self.expression, self.return_keyword, symbol)
            if symbol and symbol.memory_info.ast_consumed:
                raise SemanticErrors.USING_NON_INITIALIZED_VALUE(self, symbol)
            if symbol and symbol.memory_info.ast_initialized:
                symbol.memory_info.ast_consumed = True

        # Check the return type matches the enclosing function's return type.
        target_return_type = kwargs["target-return-type"]
        actual_return_type = self.expression.infer_type(scope_handler, **kwargs).type if self.expression else CommonTypes.void()
        if not actual_return_type.symbolic_eq(target_return_type, scope_handler.current_scope):
            symbol = scope_handler.current_scope.get_outermost_variable_symbol(self.expression)
            raise SemanticErrors.TYPE_MISMATCH(self.expression or self.return_keyword, actual_return_type, target_return_type, symbol)


__all__ = ["ReturnStatementAst"]

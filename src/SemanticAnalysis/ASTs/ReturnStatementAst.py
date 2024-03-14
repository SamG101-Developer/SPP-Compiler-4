from dataclasses import dataclass
from typing import Optional

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Types.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from src.SemanticAnalysis.ASTs.Meta.AstUtils import AstUtils

from src.SemanticAnalysis.ASTs.TokenAst import TokenAst
from src.SemanticAnalysis.ASTs.ExpressionAst import ExpressionAst
from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst


@dataclass
class ReturnStatementAst(Ast, SemanticAnalysis):
    """
    The ReturnStatementAst node represents a return statement. This is a statement that returns a value from a function.
    It has a return keyword and an optional expression to return.

    Attributes:
        - return_keyword: The "ret" keyword.
        - expression: The expression to return.
    """

    return_keyword: TokenAst
    expression: Optional[ExpressionAst]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ReturnStatementAst.
        s = ""
        s += f"{self.return_keyword.print(printer)}"
        s += f"{self.expression.print(printer)}" if self.expression else ""
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # If there is a value being returned, analyse it and ensure its memory status is "owned".
        if self.expression:
            self.expression.do_semantic_analysis(scope_handler, **kwargs)
            AstUtils.ensure_memory_integrity_of_expression(self.expression, scope_handler, **kwargs)

        # Check the return type matches the function's return type.
        target_return_type = kwargs.get("target-return-type")
        return_type = self.expression.infer_type(scope_handler, **kwargs) if self.expression else (ConventionMovAst, CommonTypes.void())
        if return_type[0] != ConventionMovAst or not target_return_type.symbolic_eq(return_type[1], scope_handler.current_scope):
            exception = SemanticError(f"Returning variable of incorrect type:")
            exception.add_traceback(target_return_type.pos, f"Function has return type '{target_return_type}'.")
            exception.add_traceback(self.pos, f"Variable '{self.expression}' returned here is type '{return_type[0]}{return_type[1]}'.")
            raise exception


__all__ = ["ReturnStatementAst"]

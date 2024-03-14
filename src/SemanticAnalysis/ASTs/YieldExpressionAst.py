from dataclasses import dataclass
from typing import Optional, Tuple, Type

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler
from src.SemanticAnalysis.Types.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class YieldExpressionAst(Ast, SemanticAnalysis):
    """
    The YieldExpressionAst node is used to represent a yield expression in a coroutine (always yields to a generator
    object). Yield expressions are expression to allow "sending" a value into the coroutine with "let x = yield 5", and
    generator.next(value=1). Chaining coroutines is also supported with "yield with gen()". Conventions can be applied
    to yielded values, as control will return to the coroutine, who controls the owned object.

    Attributes:
        - yield_keyword: The "gen" keyword.
        - with_keyword: The "with" keyword.
        - convention: The optional convention of the yield expression.
        - expression: The optional expression of the yield expression.
    """

    yield_keyword: "TokenAst"
    with_keyword: Optional["TokenAst"]
    convention: "ConventionAst"
    expression: Optional["ExpressionAst"]

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the YieldExpressionAst.
        s = ""
        s += f"{self.yield_keyword.print(printer)}"
        s += f"{self.with_keyword.print(printer)}" if self.with_keyword else ""
        s += f"{self.convention.print(printer)}"
        s += f"{self.expression.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse the expression.
        self.expression.do_semantic_analysis(scope_handler, **kwargs)
        coroutine_return_type = kwargs.get("target-return-type")

        # Ensure that the return type is a Generator type
        if not coroutine_return_type.without_generics().symbolic_eq(CommonTypes.gen().without_generics(), scope_handler.current_scope):
            exception = SemanticError(f"Gen expressions can only occur inside a function that returns a Generator")
            exception.add_traceback(self.pos, f"Gen expression found here.")
            exception.add_traceback(coroutine_return_type.pos, f"Function returns type '{coroutine_return_type}'.")
            raise exception

        kwargs["coroutine"] = True

        # Ensure the return type's Yield typedef matches the expression's type
        # TODO: required typedefs to be implemented first


__all__ = ["YieldExpressionAst"]

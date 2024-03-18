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

    The convention of the yielded value must match the generator type (ie yield &variable for GenRef[...]) etc. This is
    how the convention of coroutine yielding is enforced.

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
        from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

        # Analyse the expression.
        self.expression.do_semantic_analysis(scope_handler, **kwargs)
        coroutine_return_type = kwargs.get("target-return-type")

        # Ensure that the return type is a Generator type. TODO: change to std. ...
        if coroutine_return_type.parts[-1].value not in ["GenMov", "GenRef", "GenMut"]:
            exception = SemanticError(f"Gen expressions can only occur inside a function that returns a Generator")
            exception.add_traceback(self.pos, f"Gen expression found here.")
            exception.add_traceback(coroutine_return_type.pos, f"Function returns type '{coroutine_return_type}'.")
            raise exception

        kwargs["coroutine"] = True

        # Determine the given yield type and convention (if the expression is a parameter variable it could have an
        # implicit convention)
        given_yield_type = self.expression.infer_type(scope_handler, **kwargs)[1]
        given_convention = None
        match self.convention, self.expression.infer_type(scope_handler, **kwargs)[0]:
            case ConventionMovAst(), that_convention: given_convention = that_convention
            case self_convention, _: given_convention = type(self_convention)

        # Determine the expected yield type and convention. The expected yield type is the "Yield" generic parameter's
        # argument, and the expected convention is determined from the "Gen[Mov|Ref|Mut]" type.
        expected_yield_type = coroutine_return_type.parts[-1].generic_arguments["Yield"]
        expected_convention = CommonTypes.type_variant_to_convention(coroutine_return_type.parts[-1])

        # Check that the convention-type pairs match.
        if not expected_yield_type.symbolic_eq(given_yield_type, scope_handler.current_scope) or not isinstance(expected_convention, given_convention):
            exception = SemanticError(f"Invalid yield type from coroutine:")
            exception.add_traceback(expected_yield_type.pos, f"Coroutine yield type specified here as '{expected_convention}{expected_yield_type}'.")
            exception.add_traceback_minimal(self.expression.pos, f"Yield expression found here with type: '{given_convention}{given_yield_type}'.")
            raise exception


__all__ = ["YieldExpressionAst"]

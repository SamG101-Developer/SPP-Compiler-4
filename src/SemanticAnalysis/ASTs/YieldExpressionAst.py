from dataclasses import dataclass
from typing import Optional

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorStringFormatType, SemanticErrorType
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *


@dataclass
class YieldExpressionAst(Ast, SemanticAnalyser):
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

        # Mark the function as a coroutine.
        kwargs["fn-proto"]._is_coro = True

        # Analyse the expression.
        self.expression.do_semantic_analysis(scope_handler, **kwargs)
        coroutine_return_type = kwargs.get("target-return-type")

        # Ensure that the return type is a Generator type. TODO: change to std. ...
        if coroutine_return_type.parts[-1].value not in ["GenMov", "GenRef", "GenMut"]:
            exception = SemanticError()
            exception.add_info(
                pos=coroutine_return_type.pos,
                tag_message=f"Function type defined as '{coroutine_return_type}'")
            exception.add_error(
                pos=self.pos,
                error_type=SemanticErrorType.TYPE_ERROR,
                message="Gen expressions can only occur inside a generator",
                tag_message=f"Gen expression found here",
                tip="Ensure the function returns a 'GenMov', 'GenRef' or 'GenMut' type.")
            raise exception

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

        # Check the convention-type pairs match.
        if not expected_yield_type.symbolic_eq(given_yield_type, scope_handler.current_scope) or not isinstance(expected_convention, given_convention):
            exception = SemanticError()
            exception.add_info(
                pos=expected_yield_type.pos,
                tag_message=f"Coroutine yield type defined as '{expected_yield_type}'")
            exception.add_error(
                pos=self.pos,
                error_type=SemanticErrorType.TYPE_ERROR,
                message="Yield expression does not match the coroutine's yield type",
                tag_message=f"Yield expression inferred as '{given_convention}{given_yield_type}'",
                tip="Ensure the yield expression matches the coroutine's yield type.")
            raise exception

        # TODO:
        #  - Mark symbol as moved / partially moved for GenMov generators.
        #  - Mark symbol as borrowed for Gen[Ref|Mut] generators.
        #  - Enforce the Law of Exclusivity for Gen[Ref|Mut] generators (within tuples).


__all__ = ["YieldExpressionAst"]

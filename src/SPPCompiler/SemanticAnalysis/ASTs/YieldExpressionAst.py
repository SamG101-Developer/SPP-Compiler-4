from dataclasses import dataclass
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.Utils.Sequence import Seq


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
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # Ensure the yield expression is in a coroutine.
        if "is-coroutine" not in kwargs:
            raise SemanticErrors.YIELD_OUTSIDE_COROUTINE(self, kwargs["is-subroutine"])

        # Analyse the expression.
        self.expression.do_semantic_analysis(scope_handler, **kwargs)
        coroutine_ret_type = kwargs["target-return-type"]

        # Determine the yield's convention and type.
        match self.convention, self.expression.infer_type(scope_handler, **kwargs).convention:
            case ConventionMovAst(), that_convention: given_yield_convention = that_convention
            case self_convention, _: given_yield_convention = type(self.convention)

        given_yield_type = InferredType(
            convention=given_yield_convention,
            type=self.expression.infer_type(scope_handler, **kwargs).type)

        # Check the yielded convention and type matches the coroutine's return type.
        target_yield_type = InferredType(
            convention=CommonTypes.type_variant_to_convention(coroutine_ret_type.parts[-1]),
            type=Seq(coroutine_ret_type.parts[-1].generic_arguments.arguments).find(lambda i: i.identifier.parts[-1].to_identifier().value == "Yield").type)

        if not given_yield_type.symbolic_eq(target_yield_type, scope_handler):
            raise SemanticErrors.TYPE_MISMATCH(self, given_yield_type, target_yield_type)


__all__ = ["YieldExpressionAst"]

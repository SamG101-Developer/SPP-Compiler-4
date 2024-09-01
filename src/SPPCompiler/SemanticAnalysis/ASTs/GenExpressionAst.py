from dataclasses import dataclass, field
from typing import Optional

from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import InferredType, TypeInfer
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes


@dataclass
class GenExpressionAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The YieldExpressionAst node is used to represent a yield expression in a coroutine (always yields to a generator
    object). Yield expressions are expression to allow "sending" a value into the coroutine with "let x = yield 5", and
    generator.next(value=1). Chaining coroutines is also supported with "yield with gen()". Conventions can be applied
    to yielded values, as control will return to the coroutine, who controls the owned object.

    The convention of the yielded value must match the generator type (ie yield &variable for GenRef[...]) etc. This is
    how the convention of coroutine yielding is enforced.

    Attributes:
        gen_keyword: The "gen" keyword.
        with_keyword: The "with" keyword.
        convention: The optional convention of the yield expression.
        expression: The optional expression of the yield expression.
    """

    gen_keyword: "TokenAst"
    with_keyword: Optional["TokenAst"]
    convention: "ConventionAst"
    expression: Optional["ExpressionAst"]

    _coro_type: Optional["TypeAst"] = field(default=None, init=False, repr=False)

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the YieldExpressionAst.
        s = ""
        s += f"{self.gen_keyword.print(printer)}"
        s += f"{self.with_keyword.print(printer)}" if self.with_keyword else ""
        s += f"{self.convention.print(printer)}"
        s += f"{self.expression.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst, FunctionArgumentNormalAst
        from SPPCompiler.LexicalAnalysis.Lexer import Lexer
        from SPPCompiler.SyntacticAnalysis.Parser import Parser

        # Ensure the yield expression is in a coroutine.
        if "is-subroutine" in kwargs:
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
        expected_yield_type = InferredType(
            convention=CommonTypes.type_variant_to_convention(coroutine_ret_type.types[-1]),
            type=coroutine_ret_type.types[-1].generic_arguments["Yield"].type)

        if not given_yield_type.symbolic_eq(expected_yield_type, scope_handler.current_scope):
            raise SemanticErrors.TYPE_MISMATCH_2(None, self.expression, expected_yield_type, given_yield_type, scope_handler)

        # Apply the FunctionArgumentGroup memory checks to the yielded values.
        mock_function_argument_group = Parser(Lexer("()").lex(), "").parse_function_call_arguments().parse_once()
        mock_function_argument_group.arguments = [FunctionArgumentNormalAst(self.expression.pos, self.convention, None, self.expression)]
        mock_function_argument_group.do_semantic_analysis(scope_handler, **kwargs)

        self._coro_type = coroutine_ret_type

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        # The yield expression's typeis based on the data being sent into the coroutine (the "Send" generic parameter).
        expected_send_type = self._coro_type.types[-1].generic_arguments["Send"].type
        return InferredType(convention=ConventionMovAst, type=expected_send_type)


__all__ = ["GenExpressionAst"]

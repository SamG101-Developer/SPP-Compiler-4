from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class RelStatementAst(Ast, SemanticAnalyser):
    """
    The RelStatementAst node represents the "unpinning" of a number of variables or attributes. Unpinning them allows
    them to be moved again. This allows values to be used after being borrowed for async functions and coroutines
    (adhering to the memory model).

    Only exact expressions that have been pinned can be unpinned. Therefore, if "a" was pinned, "a.x" cannot be
    unpinned, without unpinning the entirety of "a". This is because if "a" has ben moved borrowed, "a.x" might still
    want to be read. Therefore, exact matches must be unpinned.

    Attributes:
        rel_keyword: The "rel" keyword.
        expressions: The expressions to pin.
    """

    rel_keyword: "TokenAst"
    expressions: List["ExpressionAst"]

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # Analyse each expression.
        expressions = Seq(self.expressions)
        expressions.for_each(lambda e: e.do_semantic_analysis(scope_handler, **kwargs))

        # Ensure each expression is a variable or attribute: has a symbol.
        symbols = expressions.map(scope_handler.current_scope.get_outermost_variable_symbol)
        if symbols.filter_out_none().length < expressions.length:
            raise SemanticErrors.INVALID_PIN_TARGET(self, expressions[symbols.index(None)])

        # Only allow unpinning exact expressions that have been pinned.
        symbols.remove_none()
        for pin_target in expressions:
            symbol = scope_handler.current_scope.get_outermost_variable_symbol(pin_target)

            match = False
            for existing_pin in symbol.memory_info.ast_pins:
                if str(pin_target) == str(existing_pin):
                    match = True
                    break

            if not match:
                raise SemanticErrors.UNPINNING_NON_PINNED(self, pin_target)
            symbol.memory_info.ast_pins.remove(pin_target)

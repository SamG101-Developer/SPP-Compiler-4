from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class TupleLiteralAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The TupleLiteralAst node is used to represent a fixed-size collection of elements, which can be any type. Tuples
    have special operations, such as indexing, unpacking, destructuring and more.

    Attributes:
        paren_l_token: The left parenthesis token.
        items: The items that compose the tuple.
        paren_r_token: The right parenthesis token.
    """

    paren_l_token: "TokenAst"
    items: List["ExpressionAst"]
    paren_r_token: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the TupleLiteralAst.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.items).print(printer, ", ")}"
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        for item in self.items:
            # Analyse the item in the tuple
            item.do_semantic_analysis(scope_handler, **kwargs)
            symbol = scope_handler.current_scope.get_outermost_variable_symbol(item)

            # Make sure the item is not uninitialised.
            if symbol and symbol.memory_info.ast_consumed:
                raise SemanticErrors.USING_NON_INITIALIZED_VALUE(self, symbol)

            # Make sure the item is not partially moved.
            if symbol and symbol.memory_info.ast_partial_moves:
                raise SemanticErrors.USING_PARTIAL_MOVED_VALUE(self, symbol)

            # Make sure the item is not borrowed.
            if symbol and symbol.memory_info.is_borrow:
                raise SemanticErrors.MOVING_FROM_BORROWED_CONTEXT(self, item, symbol)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

        # The tuple's type is `std.Tup[...Ts]` where `Ts` is the collection of types in the tuple.
        tuple_type = CommonTypes.tuple(Seq(self.items).map(lambda i: i.infer_type(scope_handler, **kwargs).type).value, pos=self.pos)
        tuple_type.do_semantic_analysis(scope_handler, **kwargs)
        return InferredType(convention=ConventionMovAst, type=tuple_type)


__all__ = ["TupleLiteralAst"]

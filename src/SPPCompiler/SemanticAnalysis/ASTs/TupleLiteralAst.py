from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType, ensure_memory_integrity
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
            ensure_memory_integrity(self, item, item, scope_handler)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

        # The tuple's type is `std.Tup[...Ts]` where `Ts` is the collection of types in the tuple.
        tuple_type = CommonTypes.tuple(Seq(self.items).map(lambda i: i.infer_type(scope_handler, **kwargs).type).list(), pos=self.pos)
        tuple_type.do_semantic_analysis(scope_handler, **kwargs)
        return InferredType(convention=ConventionMovAst, type=tuple_type)


__all__ = ["TupleLiteralAst"]

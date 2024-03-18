from dataclasses import dataclass
from typing import List, Tuple, Type

from src.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from src.SemanticAnalysis.Utils.Scopes import ScopeHandler
from src.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from src.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst

from src.Utils.Sequence import Seq


@dataclass
class TupleLiteralAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The TupleLiteralAst node is used to represent a fixed-size collection of elements, which can be any type. Tuples
    have special operations, such as indexing, unpacking, destructuring and more.

    Attributes:
        - paren_l_token: The left parenthesis token.
        - items: The items that compose the tuple.
        - paren_r_token: The right parenthesis token.
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
        # Analyse the items of the tuple.
        Seq(self.items).for_each(lambda i: i.do_semantic_analysis(scope_handler, **kwargs))

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> Tuple[Type["ConventionAst"], "TypeAst"]:
        # The tuple's type is `std.Tup[...Ts]` where `Ts` is the collection of types in the tuple.
        tuple_type = CommonTypes.tuple(Seq(self.items).map(lambda i: i.infer_type(scope_handler, **kwargs)[1]).value, pos=self.pos)
        tuple_type.do_semantic_analysis(scope_handler, **kwargs)
        return ConventionMovAst, tuple_type


__all__ = ["TupleLiteralAst"]

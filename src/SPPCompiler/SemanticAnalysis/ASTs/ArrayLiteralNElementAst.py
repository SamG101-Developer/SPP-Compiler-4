from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType, ensure_memory_integrity
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ArrayLiteralNElementAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The ArrayLiteral0ElementAst node is used to represent an array literal with no elements.

    Attributes:
        left_bracket: The left bracket token.
        items: The elements inside the array.
        right_bracket: The right bracket token.
    """

    left_bracket: "TokenAst"
    items: List["ExpressionAst"]
    right_bracket: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ArrayLiteral0ElementAst.
        s = ""
        s += f"{self.left_bracket.print(printer)}"
        s += f"{Seq(self.items).print(printer, ", ")}"
        s += f"{self.right_bracket.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors

        # Analyse the items inside the array.
        for item in self.items:
            item.do_semantic_analysis(scope_handler, **kwargs)

        # Ensure all the types are the same.
        unique_types = Seq(self.items).map(lambda x: x.infer_type(scope_handler).type).unique_items()
        if unique_types.length > 1:
            raise SemanticErrors.ARRAY_ELEMENT_TYPE_MISMATCH(unique_types[0], unique_types[1])

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst

        # An empty array's generic type is the given type between the brackets.
        element_type = self.items[0].infer_type(scope_handler).type
        array_type = CommonTypes.arr(element_type, self.pos)
        array_type.do_semantic_analysis(scope_handler, **kwargs)
        return InferredType(convention=ConventionMovAst, type=array_type)


__all__ = ["ArrayLiteralNElementAst"]

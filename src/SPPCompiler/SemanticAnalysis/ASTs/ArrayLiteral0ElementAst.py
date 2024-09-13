from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler


@dataclass
class ArrayLiteral0ElementAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The ArrayLiteral0ElementAst node is used to represent an array literal with no elements.

    Attributes:
        left_bracket: The left bracket token.
        type: The element type inside the array.
        right_bracket: The right bracket token.
    """

    left_bracket: "TokenAst"
    type: "TypeAst"
    right_bracket: "TokenAst"

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the ArrayLiteral0ElementAst.
        s = ""
        s += f"{self.left_bracket.print(printer)}"
        s += f"{self.type.print(printer)}"
        s += f"{self.right_bracket.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler, **kwargs) -> None:
        # Analyse the type inside the brackets.
        self.type.do_semantic_analysis(scope_handler, **kwargs)

    def infer_type(self, scope_handler: ScopeHandler, **kwargs) -> InferredType:
        # An empty array's generic type is the given type between the brackets.
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst
        array_type = CommonTypes.arr(self.type, self.pos)
        array_type.do_semantic_analysis(scope_handler, **kwargs)
        return InferredType(convention=ConventionMovAst, type=array_type)


__all__ = ["ArrayLiteral0ElementAst"]

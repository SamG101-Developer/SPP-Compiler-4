from dataclasses import dataclass

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstUtils import TypeInfer, InferredType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes


@dataclass
class BooleanLiteralAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The BooleanLiteralAst node represents a boolean value, which can be either `true` or `false`.

    Attributes:
        boolean: The boolean value.
    """

    boolean: bool

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the BooleanLiteralAst.
        s = ""
        s += f"{"true" if self.boolean else "false"}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        assert isinstance(self.boolean, bool)
        assert self.boolean in [True, False]

    def infer_type(self, scope_handler, **kwargs) -> InferredType:
        from SPPCompiler.SemanticAnalysis.ASTs import ConventionMovAst
        return InferredType(convention=ConventionMovAst, type=CommonTypes.bool(self.pos))


__all__ = ["BooleanLiteralAst"]

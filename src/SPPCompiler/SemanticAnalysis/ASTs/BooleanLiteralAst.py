from dataclasses import dataclass
from typing import Tuple, Type

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.CommonTypes import CommonTypes
from SPPCompiler.SemanticAnalysis.ASTMixins.TypeInfer import TypeInfer

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.ConventionAst import ConventionAst
from SPPCompiler.SemanticAnalysis.ASTs.ConventionMovAst import ConventionMovAst
from SPPCompiler.SemanticAnalysis.ASTs.TypeAst import TypeAst


@dataclass
class BooleanLiteralAst(Ast, SemanticAnalyser, TypeInfer):
    """
    The BooleanLiteralAst node represents a boolean value, which can be either `true` or `false`.

    Attributes:
        - boolean: The boolean value.
    """

    boolean: bool

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the BooleanLiteralAst.
        s = ""
        s += f"{"true" if self.boolean else "false"}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    def infer_type(self, scope_handler, **kwargs) -> Tuple[Type[ConventionAst], TypeAst]:
        # A boolean is always of type `std.Bool`.
        return ConventionMovAst, CommonTypes.bool(self.pos)


__all__ = ["BooleanLiteralAst"]

from dataclasses import dataclass
from typing import List

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst
from SPPCompiler.SemanticAnalysis.ASTs.ObjectInitializerArgumentAst import ObjectInitializerArgumentAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class ObjectInitializerArgumentGroupAst(Ast, SemanticAnalyser):
    """
    The ObjectInitializerArgumentGroupAst node is used to represent the arguments of an object initializer. It contains
    named and normal arguments of the object initializer.

    Attributes:
        - paren_l_token: The left parenthesis token.
        - arguments: The arguments of the object initializer.
        - paren_r_token: The right parenthesis token.
    """

    paren_l_token: TokenAst
    arguments: List[ObjectInitializerArgumentAst]
    paren_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the object initializer argument group.
        s = ""
        s += f"{self.paren_l_token.print(printer)}"
        s += f"{Seq(self.arguments).print(printer, ", ")}" if self.arguments else ""
        s += f"{self.paren_r_token.print(printer)}"
        return s

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...


__all__ = ["ObjectInitializerArgumentGroupAst"]

from __future__ import annotations
from dataclasses import dataclass
from typing import List

from SPPCompiler.LexicalAnalysis.Tokens import TokenType

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.SemanticAnalysis.Utils.Symbols import TypeSymbol

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast, Default
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.GenericParameterAst import GenericParameterAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericParameterRequiredAst import GenericParameterRequiredAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericParameterOptionalAst import GenericParameterOptionalAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericParameterVariadicAst import GenericParameterVariadicAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class GenericParameterGroupAst(Ast, Default, SemanticAnalyser):
    """
    The GenericParameterGroupAst node is used to represent a group of generic parameters in a
    function/class/superimposition prototype, NOT the arguments to a function call or object instantiation (see
    GenericArgumentGroupAst).

    Attributes:
        - bracket_l_token: The left bracket token.
        - parameters: The generic parameters of the function/class/superimposition prototype.
        - bracket_r_token: The right bracket token.
    """

    bracket_l_token: TokenAst
    parameters: List[GenericParameterAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the GenericParameterGroupAst.
        s = ""
        if self.parameters:
            s += f"{self.bracket_l_token.print(printer)}"
            s += Seq(self.parameters).print(printer, ", ")
            s += f"{self.bracket_r_token.print(printer)}"
        return s

    @staticmethod
    def default() -> GenericParameterGroupAst:
        # Create a default GenericParameterGroupAst.
        return GenericParameterGroupAst(
            pos=-1,
            bracket_l_token=TokenAst.dummy(TokenType.TkBrackL),
            parameters=[],
            bracket_r_token=TokenAst.dummy(TokenType.TkBrackR))

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    def get_req(self) -> List[GenericParameterRequiredAst]:
        # Get all the required generic parameters.
        return [p for p in self.parameters if isinstance(p, GenericParameterRequiredAst)]

    def get_opt(self) -> List[GenericParameterOptionalAst]:
        # Get all the optional generic parameters.
        return [p for p in self.parameters if isinstance(p, GenericParameterOptionalAst)]

    def get_var(self) -> List[GenericParameterVariadicAst]:
        # Get all the variadic generic parameters.
        return [p for p in self.parameters if isinstance(p, GenericParameterVariadicAst)]


__all__ = ["GenericParameterGroupAst"]

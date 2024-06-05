from __future__ import annotations
from dataclasses import dataclass
from typing import List, Optional

from SPPCompiler.LexicalAnalysis.Tokens import TokenType

from SPPCompiler.SemanticAnalysis.ASTMixins.SemanticAnalyser import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticError, SemanticErrorType
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler

from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast, Default
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from SPPCompiler.SemanticAnalysis.ASTs.GenericArgumentAst import GenericArgumentAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericArgumentNamedAst import GenericArgumentNamedAst
from SPPCompiler.SemanticAnalysis.ASTs.GenericArgumentNormalAst import GenericArgumentNormalAst
from SPPCompiler.SemanticAnalysis.ASTs.TokenAst import TokenAst

from SPPCompiler.Utils.Sequence import Seq


@dataclass
class GenericArgumentGroupAst(Ast, Default, SemanticAnalyser):
    """
    The GenericArgumentGroupAst node is used to group the generic arguments to a function call or object initialization,
    NOT the generic parameters of a function/class/superimposition prototype (see GenericParameterGroupAst).

    Attributes:
        - bracket_l_token: The left bracket token.
        - arguments: The generic arguments.
        - bracket_r_token: The right bracket token.
    """

    bracket_l_token: TokenAst
    arguments: List[GenericArgumentAst]
    bracket_r_token: TokenAst

    @ast_printer_method
    def print(self, printer: AstPrinter) -> str:
        # Print the GenericArgumentGroupAst.
        s = ""
        if self.arguments:
            s += f"{self.bracket_l_token.print(printer)}"
            s += f"{Seq(self.arguments).print(printer, ", ")}"
            s += f"{self.bracket_r_token.print(printer)}"
        return s

    @staticmethod
    def default() -> GenericArgumentGroupAst:
        # Create a default GenericArgumentGroupAst.
        return GenericArgumentGroupAst(
            pos=-1,
            bracket_l_token=TokenAst.dummy(TokenType.TkBrackL),
            arguments=[],
            bracket_r_token=TokenAst.dummy(TokenType.TkBrackR))

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        ...

    def __getitem__(self, item: str) -> Optional["TypeAst"]:
        from SPPCompiler.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst

        mock_identifier = IdentifierAst(-1, item)
        for argument in self.arguments:
            if isinstance(argument, GenericArgumentNamedAst) and argument.identifier.parts[-1].value == mock_identifier.value:
                return argument.type

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same arguments.
        return isinstance(other, GenericArgumentGroupAst) and self.arguments == other.arguments

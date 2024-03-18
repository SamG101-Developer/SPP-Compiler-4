from __future__ import annotations
from dataclasses import dataclass
from typing import List, Optional

from src.LexicalAnalysis.Tokens import TokenType

from src.SemanticAnalysis.Analysis.SemanticAnalysis import SemanticAnalysis
from src.SemanticAnalysis.Analysis.SemanticError import SemanticError
from src.SemanticAnalysis.Symbols.Scopes import ScopeHandler

from src.SemanticAnalysis.ASTs.Meta.Ast import Ast, Default
from src.SemanticAnalysis.ASTs.Meta.AstPrinter import *

from src.SemanticAnalysis.ASTs.GenericArgumentAst import GenericArgumentAst
from src.SemanticAnalysis.ASTs.GenericArgumentNamedAst import GenericArgumentNamedAst
from src.SemanticAnalysis.ASTs.GenericArgumentNormalAst import GenericArgumentNormalAst
from src.SemanticAnalysis.ASTs.TokenAst import TokenAst

from src.Utils.Sequence import Seq


@dataclass
class GenericArgumentGroupAst(Ast, Default, SemanticAnalysis):
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
        # Check no named arguments have the same name
        named_arguments = Seq(self.arguments).filter(lambda a: isinstance(a, GenericArgumentNamedAst)).map(lambda a: a.identifier)
        if named_arguments.contains_duplicates():
            duplicate_named_arguments = named_arguments.non_unique_items()[0]
            exception = SemanticError(f"Duplicate generic argument names '{duplicate_named_arguments[0]}' found:")
            exception.add_traceback(duplicate_named_arguments[0].pos, f"Argument <{duplicate_named_arguments[0]}> declared here.")
            exception.add_traceback(duplicate_named_arguments[1].pos, f"Argument <{duplicate_named_arguments[1]}> re-declared here.")
            raise exception

        # Check argument order is Normal -> Named
        ordering = {GenericArgumentNormalAst: "Normal", GenericArgumentNamedAst: "Named"}
        current_classifications = Seq(self.arguments).map(lambda p: (type(p), p))
        sorted_classifications = current_classifications.sort(key=lambda t: list(ordering.keys()).index(t[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            exception = SemanticError(f"Invalid generic argument order:")
            exception.add_traceback(difference[-2][1].pos, f"{ordering[difference[-2][0]]} argument '{difference[-2][1]}' declared here.")
            exception.add_traceback(difference[-1][1].pos, f"{ordering[difference[-1][0]]} argument '{difference[-1][1]}' declared here.")
            raise exception

        # Analyse each argument.
        Seq(self.arguments).for_each(lambda p: p.do_semantic_analysis(scope_handler, **kwargs))

    def __getitem__(self, item: str) -> Optional["TypeAst"]:
        from src.SemanticAnalysis.ASTs.IdentifierAst import IdentifierAst

        mock_identifier = IdentifierAst(-1, item)
        for argument in self.arguments:
            if isinstance(argument, GenericArgumentNamedAst) and argument.identifier.parts[-1].value == mock_identifier.value:
                return argument.type

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same arguments.
        return isinstance(other, GenericArgumentGroupAst) and self.arguments == other.arguments

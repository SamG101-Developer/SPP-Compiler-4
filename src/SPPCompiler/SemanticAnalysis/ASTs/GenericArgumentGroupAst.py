from __future__ import annotations
from dataclasses import dataclass
from typing import Dict, List

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast, Default
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class GenericArgumentGroupAst(Ast, Default, SemanticAnalyser):
    """
    The GenericArgumentGroupAst node is used to group the generic arguments to a function call or object initialization,
    NOT the generic parameters of a function/class/superimposition prototype (see GenericParameterGroupAst).

    Attributes:
        bracket_l_token: The left bracket token.
        arguments: The generic arguments.
        bracket_r_token: The right bracket token.
    """

    bracket_l_token: "TokenAst"
    arguments: List["GenericArgumentAst"]
    bracket_r_token: "TokenAst"

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
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst

        # Create a default GenericArgumentGroupAst.
        return GenericArgumentGroupAst(
            pos=-1,
            bracket_l_token=TokenAst.dummy(TokenType.TkBrackL),
            arguments=[],
            bracket_r_token=TokenAst.dummy(TokenType.TkBrackR))

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        # This method sometimes needs to be called to ensure certain checks, but the rest of the semantic analysis also
        # handles the symbols' memory state, which isn't desired here.
        from SPPCompiler.SemanticAnalysis.ASTs import GenericArgumentNormalAst, GenericArgumentNamedAst

        # Check there are no duplicate named-argument identifiers for this group, and raise an exception if there are.
        named_arguments = Seq(self.arguments).filter_to_type(GenericArgumentNamedAst).map(lambda a: a.identifier)
        if named_arguments.contains_duplicates():
            duplicate_named_argument_identifiers = named_arguments.non_unique_items()[0]
            raise SemanticErrors.DUPLICATE_ITEM(duplicate_named_argument_identifiers, "named generic argument")

        # Ensure the ordering of arguments in this group is correct (Normal => Named).
        classification_ordering = {GenericArgumentNormalAst: "Anonymous", GenericArgumentNamedAst: "Named"}
        current_classifications = Seq(self.arguments).map(type).zip(Seq(self.arguments))
        sorted_classifications  = current_classifications.sort(key=lambda c: list(classification_ordering.keys()).index(c[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            raise SemanticErrors.INVALID_ORDER(difference.value, classification_ordering, "generic argument")

        # Analyse each generic argument.
        Seq(self.arguments).for_each(lambda a: a.do_semantic_analysis(scope_handler, **kwargs))

    def __eq__(self, other):
        # Check both ASTs are the same type and have the same arguments.
        return self.arguments == other.arguments

    @staticmethod
    def from_dict(dictionary: Dict["TypeAst", "TypeAst"]) -> GenericArgumentGroupAst:
        from SPPCompiler.SemanticAnalysis.ASTs import GenericArgumentNamedAst, TokenAst

        arguments = [GenericArgumentNamedAst(
            pos=-1,
            raw_identifier=identifier.parts[-1].to_identifier(),
            assignment_token=TokenAst.dummy(TokenType.TkAssign),
            type=type) for identifier, type in dictionary.items()]

        return GenericArgumentGroupAst(
            pos=-1,
            bracket_l_token=TokenAst.dummy(TokenType.TkBrackL),
            arguments=arguments,
            bracket_r_token=TokenAst.dummy(TokenType.TkBrackR))

    @staticmethod
    def from_list(list: List["GenericArgumentAst"]) -> GenericArgumentGroupAst:
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst

        return GenericArgumentGroupAst(
            pos=-1,
            bracket_l_token=TokenAst.dummy(TokenType.TkBrackL),
            arguments=list,
            bracket_r_token=TokenAst.dummy(TokenType.TkBrackR))

    def __copy__(self):
        return GenericArgumentGroupAst(self.pos, self.bracket_l_token, self.arguments.copy(), self.bracket_r_token)


__all__ = ["GenericArgumentGroupAst"]

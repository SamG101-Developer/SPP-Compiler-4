from __future__ import annotations

from dataclasses import dataclass
from typing import List

from SPPCompiler.LexicalAnalysis.Tokens import TokenType
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstMixins import SemanticAnalyser
from SPPCompiler.SemanticAnalysis.ASTs.Meta.Ast import Ast, Default
from SPPCompiler.SemanticAnalysis.ASTs.Meta.AstPrinter import *
from SPPCompiler.SemanticAnalysis.Utils.SemanticError import SemanticErrors
from SPPCompiler.SemanticAnalysis.Utils.Scopes import ScopeHandler
from SPPCompiler.Utils.Sequence import Seq


@dataclass
class GenericParameterGroupAst(Ast, Default, SemanticAnalyser):
    """
    The GenericParameterGroupAst node is used to represent a group of generic parameters in a
    function/class/superimposition prototype, NOT the arguments to a function call or object instantiation (see
    GenericArgumentGroupAst).

    Attributes:
        bracket_l_token: The left bracket token.
        parameters: The generic parameters of the function/class/superimposition prototype.
        bracket_r_token: The right bracket token.
    """

    bracket_l_token: "TokenAst"
    parameters: List["GenericParameterAst"]
    bracket_r_token: "TokenAst"

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
        from SPPCompiler.SemanticAnalysis.ASTs import TokenAst

        # Create a default GenericParameterGroupAst.
        return GenericParameterGroupAst(
            pos=-1,
            bracket_l_token=TokenAst.dummy(TokenType.TkBrackL),
            parameters=[],
            bracket_r_token=TokenAst.dummy(TokenType.TkBrackR))

    def do_semantic_analysis(self, scope_handler: ScopeHandler, **kwargs) -> None:
        from SPPCompiler.SemanticAnalysis.ASTs import (
            GenericParameterRequiredAst, GenericParameterOptionalAst, GenericParameterVariadicAst)

        # Check there are no duplicate parameter names for this function, and raise an exception if there are.
        if Seq(self.parameters).map(lambda p: p.identifier.types[-1]).contains_duplicates():
            duplicate_parameters = Seq(self.parameters).map(lambda p: p.identifier).non_unique_items()[0]
            raise SemanticErrors.DUPLICATE_ITEM(duplicate_parameters, "parameters")

        # Ensure the ordering of parameters in this group is correct (Required => Optional => Variadic).
        classification_ordering = {
            GenericParameterRequiredAst: "Required", GenericParameterOptionalAst: "Optional",
            GenericParameterVariadicAst: "Variadic"}

        current_classifications = Seq(self.parameters).map(type).zip(Seq(self.parameters))
        sorted_classifications  = current_classifications.sort(key=lambda c: list(classification_ordering.keys()).index(c[0]))
        if current_classifications != sorted_classifications:
            difference = sorted_classifications.ordered_difference(current_classifications)
            raise SemanticErrors.INVALID_ORDER(difference.value, classification_ordering, "parameter")

    def get_req(self) -> Seq["GenericParameterRequiredAst"]:
        # Get all the required generic parameters.
        from SPPCompiler.SemanticAnalysis.ASTs import GenericParameterRequiredAst
        return Seq(self.parameters).filter_to_type(GenericParameterRequiredAst)

    def get_opt(self) -> Seq["GenericParameterOptionalAst"]:
        # Get all the optional generic parameters.
        from SPPCompiler.SemanticAnalysis.ASTs import GenericParameterOptionalAst
        return Seq(self.parameters).filter_to_type(GenericParameterOptionalAst)

    def get_var(self) -> Seq["GenericParameterVariadicAst"]:
        # Get all the variadic generic parameters.
        from SPPCompiler.SemanticAnalysis.ASTs import GenericParameterVariadicAst
        return Seq(self.parameters).filter_to_type(GenericParameterVariadicAst)

    def __copy__(self):
        return GenericParameterGroupAst(self.pos, self.bracket_l_token, self.parameters.copy(), self.bracket_r_token)


__all__ = ["GenericParameterGroupAst"]
